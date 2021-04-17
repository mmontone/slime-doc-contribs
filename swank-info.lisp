(require :closer-mop)
(require :alexandria)
(require :cl-fad)

(in-package :swank)

(defun aget (alist key)
  (cdr (assoc key alist :test 'equalp)))

(defun tboundp (symbol)
  "Returns T if a type is bound to symbol"
  ;; This was taken from swank backend
  ;; TODO: implement this portably
  (sb-int:info :type :kind symbol))

(defun read-symbol-info (symbol)
  (cond
    ((fboundp symbol)
     (load-function-info symbol))
    ((boundp symbol)
     (load-variable-info symbol))
    ((safe-class-for-symbol symbol)
     (load-class-info symbol))
    ((tboundp symbol)
     (load-type-info symbol))
    (t (error "Cannot read info of symbol: ~a" symbol))))

(defun collect-package-info (&optional (package *package*))
  (let (docs)
    (do-external-symbols (symbol package)
      (push (read-symbol-info symbol) docs))
    docs))

;; From docbrowser

(defun nice-princ-to-string (obj)
  (typecase obj
    (string obj)
    (keyword (prin1-to-string obj))
    (t (princ-to-string obj))))

#+sbcl(defmethod documentation ((slotd sb-pcl::condition-effective-slot-definition) (doc-type (eql 't)))
        "This method definition is missing in SBCL as of 1.0.55 at least. Adding it here
will make documentation for slots in conditions work properly."
        (slot-value slotd 'sb-pcl::%documentation))

(defun assoc-cdr (key data &key error-p)
  "Return (CDR (ASSOC KEY DATA)). If ERROR-P is non-NIL, signal an error if KEY is
not available is DATA."
  (let ((v (assoc key data)))
    (when (and error-p
               (not v))
      (error "~s not found in data" key))
    (cdr v)))

#+nil(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
       "Return non-NIL if SYMBOL is external in PACKAGE. SYMBOL may be either
a symbol, or a SETF form, in which case the check will be performed on
the CADR of the list."
       (eq (nth-value 1 (find-symbol (symbol-name (cond ((symbolp symbol)
                                                         symbol)
                                                        ((eq (car symbol) 'setf)
                                                         (cadr symbol))
                                                        (t
                                                         (error "Unknown symbol type: ~s" symbol))))
                                     package))
           :external))

(defun prin1-to-string-with-package (obj package)
  (let ((*package* package))
    (prin1-to-string obj)))

(defun format-argument-to-string (arg)
  (etypecase arg
    (symbol (nice-princ-to-string arg))
    (list   (mapcar #'(lambda (entry conversion) (funcall conversion entry))
                    arg (list #'(lambda (v)
                                  (if (listp v)
                                      (nice-princ-to-string (car v))
                                      (nice-princ-to-string v)))
                              #'prin1-to-string
                              #'nice-princ-to-string)))))

(defun load-type-info (symbol)
  (list (cons :name symbol)
	(cons :package (symbol-package symbol))
	(cons :type :type)
	(cons :documentation (documentation symbol 'type))))

(defun load-function-info (symbol)
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase)
                          (*package* (symbol-package symbol)))
                      #+nil(format nil "~{~a~^ ~}"
                                   (mapcar #'format-argument-to-string (swank-backend:arglist symbol))
                                   )
                      (princ-to-string (swank-backend:arglist symbol))))
        (cons :package (symbol-package symbol))
        (cons :type (cond ((macro-function symbol) :macro)
                          ((typep (symbol-function symbol) 'generic-function) :generic-function)
                          (t :function)))))

(defun load-variable-info (symbol)
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'variable))
        (cons :boundp (boundp symbol))
        (cons :value (when (boundp symbol) (prin1-to-string (symbol-value symbol))))
        (cons :constant-p (constantp symbol))
        (cons :package (symbol-package symbol))
        (cons :type :variable)))

(defun find-superclasses (class)
  (labels ((f (classes found)
             (if (and classes
                      (not (eq (car classes) (find-class 'standard-object)))
                      (not (member (car classes) found)))
                 (f (cdr classes)
                    (f (closer-mop:class-direct-superclasses (car classes))
                       (cons (car classes) found)))
                 found)))
    (f (list class) nil)))

(defun safe-class-for-symbol (symbol)
  (handler-case
      (find-class symbol)
    (error nil)))

(defun assoc-name (v)
  (assoc-cdr :name v :error-p t))

(defun specialise->symbol (spec)
  (case (caar spec)
    ((defmethod) (cadar spec))
    #+ccl((ccl::reader-method) (cadr (assoc :method (cdar spec))))
    (t nil)))

(defun load-specialisation-info (class-name)
  (let* ((ignored '(initialize-instance))
         (class (if (symbolp class-name) (find-class class-name) class-name))
         (spec (swank-backend:who-specializes class)))
    (unless (eq spec :not-implemented)
      (sort (loop
              for v in spec
              for symbol = (specialise->symbol v)
              when (and (not (member symbol ignored))
                        (symbol-external-p symbol (symbol-package (class-name class))))
                collect (list (cons :name symbol)))
            #'string< :key (alexandria:compose #'princ-to-string #'assoc-name)))))

(defun %ensure-external (symbol)
  (let ((name (cond ((symbolp symbol)
                     symbol)
                    ((and (listp symbol) (eq (car symbol) 'setf))
                     (cadr symbol))
                    (t
                     (warn "Unknown type: ~s. Expected symbol or SETF form." symbol)
                     nil))))
    (when (symbol-external-p name)
      symbol)))

(defun load-accessor-info (class slot)
  (flet ((getmethod (readerp method-list)
           (dolist (method method-list)
             (let ((name (closer-mop:generic-function-name (closer-mop:method-generic-function method))))
               (when (and (eq (type-of method) (if readerp
                                                   'closer-mop:standard-reader-method
                                                   'closer-mop:standard-writer-method))
                          (eq (closer-mop:slot-definition-name (closer-mop:accessor-method-slot-definition method))
                              (closer-mop:slot-definition-name slot)))
                 (return-from getmethod name))))))

    ;; There are several different situations we want to detect:
    ;;   1) Only a reader method: "reader FOO"
    ;;   2) Only a writer method: "writer FOO"
    ;;   3) Only a writer SETF method: "writer (SETF FOO)"
    ;;   4) A reader and a SETF method: "accessor FOO"
    ;;   5) A reader and non-SETF writer: "reader FOO, writer FOO"
    ;;
    ;; The return value from this function is an alist of the following form:
    ;;
    ;;  ((:READER . FOO-READER) (:WRITER . FOO-WRITER) (:ACCESSOR . FOO-ACCESSOR))
    ;;
    ;; Note that if :ACCESSOR is given, then it's guaranteed that neither
    ;; :READER nor :WRITER will be included.
    ;;
    ;; We start by assigning the reader and writer methods to variables
    (let* ((method-list (closer-mop:specializer-direct-methods class))
           (reader (%ensure-external (getmethod t method-list)))
           (writer (%ensure-external (getmethod nil method-list))))
      ;; Now, detect the 5 different cases, but we coalease case 2 and 3.
      (cond ((and reader (null writer))
             `((:reader . ,reader)))
            ((and (null reader) writer)
             `((:writer . ,writer)))
            ((and reader (listp writer) (eq (car writer) 'setf) (eq (cadr writer) reader))
             `((:accessor . ,reader)))
            ((and reader writer)
             `((:reader . ,reader) (:writer . ,writer)))))))

(defun load-slots (class)
  (closer-mop:ensure-finalized class)
  (flet ((load-slot (slot)
           (list (cons :name (string (closer-mop:slot-definition-name slot)))
                 (cons :documentation (swank-mop:slot-definition-documentation slot))
                 ;; The LIST call below is because the accessor lookup is wrapped
                 ;; in a FOR statement in the template.
                 (cons :accessors (let ((accessor-list (load-accessor-info class slot)))
                                    (when accessor-list
                                      (list accessor-list)))))))
    (mapcar #'load-slot (closer-mop:class-slots class))))

(defun load-class-info (class-name)
  (let ((cl (find-class class-name)))
    (list (cons :name          (class-name cl))
          (cons :documentation (documentation cl 'type))
          (cons :slots         (load-slots cl))
          ;; (cons :methods       (load-specialisation-info cl)) TODO: fix

          (cons :type :class))))

(defun %annotate-function-info (fn-info classes)
  "Append :ACCESSORP tag if the function is present as an accessor function."
  (loop
    with name = (cdr (assoc :name fn-info))
    for class-info in classes
    do (loop
         for slot-info in (cdr (assoc :slots class-info))
         do (loop
              for accessor in (cdr (assoc :accessors slot-info))
              for accessor-sym = (cdar accessor)
              when (or (and (symbolp accessor-sym) (eq accessor-sym name))
                       (and (listp accessor-sym) (eq (car accessor-sym) 'setf) (eq (cadr accessor-sym) name)))
                do (return-from %annotate-function-info (append fn-info '((:accessorp t))))))
    finally (return fn-info)))

;; docbrowser stuff ends here

(defun render-texinfo-source-for-package (package-name stream)
  (flet ((fmt (str &rest args)
           (apply #'format stream str args))
         (fmtln (str &rest args)
           (apply #'format stream str args)
           (terpri stream))
         (ln ()
           (terpri stream)))
    (let* ((package (or (find-package package-name)
                        (error "Package not found: ~a" package-name)))
           (package-info (collect-package-info package)))
      (fmtln "@setfilename ~a" package-name)
      (fmtln "@settitle ~a reference" package-name)
      (ln)
      (fmtln "@dircategory Common Lisp")
      (fmtln "@direntry")
      (fmtln "* ~a: ~a reference" package-name package-name)
      (fmtln "@end direntry")
      (ln)
      (fmtln "@titlepage")
      (fmtln "@title ~a reference" package-name)
      (fmtln "@end titlepage")
      (fmtln "@contents")
      (fmtln "@node Top")
      (fmtln "@top ~a reference" package-name)
      (fmtln "This is a reference of Common Lisp package ~a" package-name)
      (when (documentation package t)
        (ln)
        (format stream (documentation package t))
        (ln))

      (ln)
      (fmtln "@menu")
      (fmtln "* Variables::")
      (fmtln "* Functions::")
      (fmtln "* Index::")
      ;;(fmtln "* Type Index::")
      (fmtln "@end menu")
      (ln)
      (fmtln "@node Variables")
      (fmtln "@chapter Variables")
      (ln)
      (loop for info in package-info
            when (eql (aget info :type) :variable)
              do (render-info info stream)
                 (ln))
      (ln)
      (fmtln "@node Functions")
      (fmtln "@chapter Functions")
      (ln)
      (loop for info in package-info
            when (eql (aget info :type) :function)
              do (render-info info stream)
                 (ln))
      (ln)
      (fmtln "@node Index")
      (fmtln "@chapter Index")
      (fmtln "@menu")
      (fmtln "* Variable Index::")
      (fmtln "* Function Index::")
      (fmtln "@end menu")
      (fmtln "@node Variable Index")
      (fmtln "@section Variable Index")
      (fmtln "@printindex vr")
      (fmtln "@node Function Index")
      (fmtln "@section Function Index")
      (fmtln "@printindex fn")
      (fmt "@bye"))))

(defun render-info (info stream &rest args)
  (case (aget info :type)
    (:variable (apply #'render-variable-info info stream args))
    (:function (apply #'render-function-info info stream args))
    (:generic-function (apply #'render-generic-function-info info stream args))))

(defun texinfo-escape (string)
  (let ((chars
          (loop
            for char across string
            if (member char '(#\{ #\} #\@))
              collect #\@ and collect char
            else
              collect char)))
    (coerce chars 'string)))

(defun render-variable-info (info stream &key package)
  "If no PACKAGE is given, use symbol package as category"
  (if (not package)
      (format stream "@defvr ~a ~a"
              (package-name (symbol-package (aget info :name)))
              (aget info :name))
      ;; else
      (format stream "@defvar ~a" (aget info :name)))
  (terpri stream) (terpri stream)
  (when (aget info :documentation)
    (render-parsed-docstring (parse-docstring (texinfo-escape (aget info :documentation)) nil) stream))
  (terpri stream)
  (if (not package)
      (write-string "@end defvr" stream)
      (write-string "@end defvar" stream))
  (terpri stream))

(defun render-function-info (info stream &key package)
  "If no PACKAGE is given, use symbol package as category"
  (if (not package)
      (format stream "@deffn ~a ~a ~a"
              (package-name (symbol-package (aget info :name)))
              (aget info :name)
              (aget info :args))
      ;; else
      (format stream "@defun ~a ~a" (aget info :name) (aget info :args)))
  (terpri stream) (terpri stream)
  (when (aget info :documentation)
    (let* ((arg-names (list-lambda-list-args (read-from-string (aget info :args)))))
      (render-parsed-docstring
       (parse-docstring (texinfo-escape (aget info :documentation)) arg-names)
       stream)))
  (terpri stream)
  (if (not package)
      (write-string "@end deffn" stream)
      (write-string "@end defun" stream))
  (terpri stream))

(defun render-generic-function-info (info stream &key package)
  "If no PACKAGE is given, use symbol package as category"
  (if (not package)
      (format stream "@deffn ~a ~a ~a"
              (package-name (symbol-package (aget info :name)))
              (aget info :name)
              (aget info :args))
      ;; else
      (format stream "@defun ~a ~a" (aget info :name) (aget info :args)))
  (terpri stream) (terpri stream)
  (when (aget info :documentation)
    (write-string (texinfo-escape (aget info :documentation)) stream))
  (terpri stream)
  (if (not package)
      (write-string "@end deffn" stream)
      (write-string "@end defun" stream))
  (terpri stream))

(defslimefun texinfo-source-for-package (package-name)
  (with-output-to-string (s)
    (render-texinfo-source-for-package (string-upcase package-name) s)))

(defun render-texinfo-source-for-symbol (symbol stream)
  (flet ((fmt (str &rest args)
           (apply #'format stream str args))
         (fmtln (str &rest args)
           (apply #'format stream str args)
           (terpri stream))
         (ln ()
           (terpri stream)))
    (let* ((symbol-info (read-symbol-info symbol)))
      (fmtln "@setfilename ~a" symbol)
      (fmtln "@settitle ~a info" symbol)
      (ln)
      (fmtln "@dircategory Common Lisp")
      (fmtln "@direntry")
      (fmtln "* ~a: ~a" symbol symbol)
      (fmtln "@end direntry")
      (ln)
      (fmtln "@titlepage")
      (fmtln "@title ~a" symbol)
      (fmtln "@end titlepage")
      (fmtln "@contents")
      (fmtln "@node Top")
      (fmtln "@top ~a" symbol)
      (ln)
      (render-info symbol-info stream)
      (ln)
      (fmt "@bye"))))

(defun render-texinfo-source-for-symbols (title symbols stream &key package)
  (flet ((fmt (str &rest args)
           (apply #'format stream str args))
         (fmtln (str &rest args)
           (apply #'format stream str args)
           (terpri stream))
         (ln ()
           (terpri stream)))
    (fmtln "@setfilename ~a.info" title)
    (fmtln "@settitle ~a info" title)
    (ln)
    (fmtln "@dircategory Common Lisp")
    (fmtln "@direntry")
    (fmtln "* ~a: apropos ~a" title title)
    (fmtln "@end direntry")
    (ln)
    (fmtln "@titlepage")
    (fmtln "@title ~a" title)
    (fmtln "@end titlepage")
    (fmtln "@contents")
    (fmtln "@node Top")
    (fmtln "@top ~a" title)
    (ln)
    (loop for symbol in symbols
          do
             (render-info (read-symbol-info symbol) stream :package package)
             (ln))
    (ln)
    (fmt "@bye")))

(defslimefun texinfo-source-for-symbol (symbol-name)
  (with-output-to-string (s)
    (render-texinfo-source-for-symbol (read-from-string symbol-name) s)))

(defslimefun texinfo-source-for-apropos (name &optional external-only
                                              case-sensitive package)
  "Make an apropos search for Emacs. Show the result in an Info buffer."
  (let ((package (when package
                   (or (parse-package package)
                       (error "No such package: ~S" package)))))
    (let ((symbols (apropos-symbols name external-only case-sensitive package)))
      (with-output-to-string (s)
        (render-texinfo-source-for-symbols
         ;;(format nil "Apropos: ~a" name)
         name
         symbols s
         :package package)))))

(defun location-pathname (location)
  (pathname
   (cadr
    (find :file (cdr location)
          :key 'car))))

(defvar *package-source-locations* (make-hash-table)
  "A cache of packages source locations")

(defun package-source-location (package)
  (or (gethash package *package-source-locations*)
      (setf (gethash package *package-source-locations*)
            (swank/backend:find-source-location package))))

;; This function finds the packages defined from an ASDF, approximatly. And it is very slow.
(defun asdf-system-packages (system)
  (let* ((asdf-system (if (or (symbolp system)
                              (stringp system))
                          (asdf:find-system system)
                          system))
         (system-source-directory (asdf:system-source-directory asdf-system)))
    (loop for package in (list-all-packages)
          for location := (package-source-location package)
          when (and (eql (car location) :location)
                    (uiop/pathname:subpathp
                     (location-pathname location)
                     system-source-directory))
            collect package)))

(defun render-texinfo-node-for-package (package stream)
  (flet ((fmt (str &rest args)
           (apply #'format stream str args))
         (fmtln (str &rest args)
           (apply #'format stream str args)
           (terpri stream))
         (ln ()
           (terpri stream)))
    (let* ((package-info (collect-package-info package)))
      (fmtln "@node ~a" (package-name package))
      (fmtln "@chapter ~a" (package-name package))
      (fmtln "This is a reference of Common Lisp package ~a" (package-name package))
      (when (documentation package t)
        (ln)
        (format stream (documentation package t))
        (ln))

      (ln)
      (fmtln "@menu")
      (fmtln "* Variables: ~a variables." (package-name package))
      (fmtln "* Functions: ~a functions." (package-name package))
      (fmtln "@end menu")
      (ln)
      (fmtln "@node ~a variables" (package-name package))
      (fmtln "@section Variables")
      (ln)
      (loop for info in package-info
            when (eql (aget info :type) :variable)
              do (render-info info stream)
                 (ln))
      (ln)
      (fmtln "@node ~a functions" (package-name package))
      (fmtln "@section Functions")
      (ln)
      (loop for info in package-info
            when (eql (aget info :type) :function)
              do (render-info info stream)
                 (ln))
      )))

(defvar +ascii-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "All letters in 7 bit ASCII.")

(defun random-string (&optional (length 32) (alphabet +ascii-alphabet+))
  "Returns a random alphabetic string.

The returned string will contain LENGTH characters chosen from
the vector ALPHABET.
"
  (loop with id = (make-string length)
        with alphabet-length = (length alphabet)
        for i below length
        do (setf (cl:aref id i)
                 (cl:aref alphabet (random alphabet-length)))
        finally (return id)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))

(defun render-readme (readme-file stream &key (use-pandoc t))
  (flet ((render-readme-file ()
           (write-line "@node README" stream)
           (write-line "@chapter README" stream)
           (terpri stream)
           (write-string (alexandria:read-file-into-string readme-file) stream))
         (write-texinfo-line (line stream)
           (let ((result line))
             (setq result (replace-all result "@subsubsection" "@subsubsubsection"))
             (setq result (replace-all result "@subsection" "@subsubsection"))
             (setq result (replace-all result "@section" "@subsection"))
             (setq result (replace-all result "@chapter" "@section"))
             (write-line result stream))))
    (if (not use-pandoc)
        (render-readme-file)
        ;; else
        ;; try with pandoc
        (let* ((readme-texinfo-filename (pathname (format nil "/tmp/readme-~a.texi"
                                                          (random-string 10))))
               (pandoc-result (third (multiple-value-list
                                      (uiop/run-program:run-program (list "pandoc" (princ-to-string readme-file) "-o" (princ-to-string readme-texinfo-filename)))))))
          (if (zerop pandoc-result)
              ;; pandoc transformation succeeded
              (with-open-file (f readme-texinfo-filename
                                 :direction :input
                                 :external-format :utf-8)
                (write-line "@node README" stream)
                (write-line "@chapter README" stream)
                (loop with node-foundp := nil
                      for line := (read-line f nil nil)
                      while line
                      if (not node-foundp)
                        do (setf node-foundp (string= (subseq line 0 4) "@top"))
                      else
                        do (write-texinfo-line line stream)))
              ;; else, pandoc failed
              ;; just render the readme file
              (render-readme-file))))))

(defun render-texinfo-source-for-system (asdf-system stream &key (include-readme t) (use-pandoc t))
  (flet ((fmt (str &rest args)
           (apply #'format stream str args))
         (fmtln (str &rest args)
           (apply #'format stream str args)
           (terpri stream))
         (ln ()
           (terpri stream)))
    (let* ((system-name (asdf:component-name asdf-system))
           (system-packages (asdf-system-packages asdf-system))
           (readme-file (find "README"
                              (fad:list-directory (asdf:system-source-directory asdf-system))
                              :key 'pathname-name
                              :test 'string=)))
      (fmtln "@setfilename ~a" system-name)
      (fmtln "@settitle ~a system reference" system-name)
      (ln)
      (fmtln "@dircategory Common Lisp")
      (fmtln "@direntry")
      (fmtln "* ~a: ~a system reference" system-name system-name)
      (fmtln "@end direntry")
      (ln)
      (fmtln "@titlepage")
      (fmtln "@title ~a system reference" system-name)
      (fmtln "@end titlepage")
      (fmtln "@contents")
      (fmtln "@node Top")
      (fmtln "@top ~a system reference" system-name)
      (ln)
      (fmtln "This is a reference of Common Lisp ASDF system ~a" system-name)

      (when (asdf:system-description asdf-system)
        (ln)
        (write-string (asdf:system-description asdf-system) stream)
        (ln))

      (ln)
      (fmtln "@menu")
      (when (and include-readme readme-file)
        (fmtln "* README::"))
      (loop for package in system-packages
            do
               (fmtln "* ~a::" (package-name package)))
      (fmtln "* Index::")
      (fmtln "@end menu")
      (ln)

      (when (and include-readme readme-file)
        (render-readme readme-file stream :use-pandoc use-pandoc)
        (ln) (ln))

      (loop for package in system-packages
            do (render-texinfo-node-for-package package stream))

      (fmtln "@node Index")
      (fmtln "@chapter Index")
      (fmtln "@menu")
      (fmtln "* Variable Index::")
      (fmtln "* Function Index::")
      (fmtln "@end menu")
      (fmtln "@node Variable Index")
      (fmtln "@section Variable Index")
      (fmtln "@printindex vr")
      (fmtln "@node Function Index")
      (fmtln "@section Function Index")
      (fmtln "@printindex fn")
      (fmt "@bye"))))

(defslimefun texinfo-source-for-system (system-name)
  (with-output-to-string (s)
    (render-texinfo-source-for-system (asdf:find-system system-name) s)))

(defun concat-rich-text (text)
  text)

(defun make-adjustable-string (s)
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))

(defun split-string-with-delimiter (string delimiter)
  "Splits a string into a list of strings, with the delimiter still
  in the resulting list."
  (let ((words nil)
        (current-word (make-adjustable-string ""))
        (predicate (cond
                     ((characterp delimiter) (lambda (char) (eql char delimiter)))
                     ((listp delimiter) (lambda (char) (member char delimiter)))
                     ((functionp delimiter) delimiter)
                     (t (error "Invalid delimiter")))))
    (do* ((i 0 (+ i 1))
          (x (char string i) (char string i)))
         ((>= (+ i 1) (length string)) (progn (vector-push-extend x current-word) (push current-word words)))
      (if (funcall predicate x)
          (unless (string= "" current-word)
            (push current-word words)
            (push (string x) words)
            (setf current-word (make-adjustable-string "")))
          (vector-push-extend x current-word)))
    (nreverse words)))

(defun list-lambda-list-args (lambda-list)
  (multiple-value-bind (required optional rest keys aok aux keyp)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (concatenate 'list
                 required
                 (mapcar 'car optional)
                 rest
                 (mapcar 'cadar keys)
                 aux)))

(defun list-package-info-args (package-info)
  (list-lambda-list-args
   (read-from-string (aget package-info :args))))

(defun parse-docstring (docstring bound-args &key case-sensitive (package *package*))
  (let ((words (split-string-with-delimiter
                docstring
                (lambda (char)
                  (member char '(#\space #\newline #\tab)))))
        (string-test (if case-sensitive
                         'string=
                         'equalp)))
    (concat-rich-text
     (loop for word in words
           collect (cond
                     ((member (string-upcase word) (mapcar 'symbol-name bound-args) :test string-test)
                      (list :arg word))
                     ((fboundp (intern (string-upcase word) package))
                      (list :fn word))
                     ((boundp (intern (string-upcase word) package))
                      (list :var word))
                     ((eql (aref word 0) #\:)
                      (list :key word))
                     (t word))))))

;; (parse-docstring "asdf" nil)
;; (parse-docstring "asdf" '(asdf))
;; (parse-docstring "funcall parse-docstring" nil)
;; (parse-docstring "adsfa adf
;; asdfasd" nil)
;;       (parse-docstring "lala :lolo" nil)
;;       (parse-docstring "*communication-style*" nil)      

(defun render-parsed-docstring (docstring stream)
  (loop for word in docstring
        do
           (cond
             ((stringp word) (write-string word stream))
             ((and (listp word) (eql (car word) :arg))
              (format stream "@var{~a}" (second word)))
             ((and (listp word) (eql (car word) :fn))
              (format stream "@ref{~a}" (second word)))
             ((and (listp word) (eql (car word) :var))
              (format stream "@ref{~a}" (second word)))
             ((and (listp word) (eql (car word) :key))
              (format stream "@var{~a}" (second word))))))

;; (render-parsed-docstring (parse-docstring "lala :lolo" nil) t)
;; (render-parsed-docstring (parse-docstring "funcall parse-docstring" nil) t)
;; (render-parsed-docstring (parse-docstring "asdf" '(asdf)) t)

(defun read-elisp-symbol-info (symbol)
  (let ((info (read-symbol-info symbol)))
    (when (aget info :package)
      (setf (cdr (assoc :package info))
            (package-name (aget info :package))))
    (when (aget info :documentation)
      (push (cons :parsed-documentation
                  (parse-docstring (aget info :documentation) nil))
            info))
    (push (cons :symbol (cdr (assoc :name info))) info)
    (setf (cdr (assoc :name info)) (symbol-name (cdr (assoc :name info))))
    info))

(defun read-elisp-package-info (package-name)
  (let ((package (or (find-package package-name)
		     (error "Package not found: ~a" package-name)))
	symbol-infos)
    (do-external-symbols (symbol package)
      (push (read-elisp-symbol-info symbol) symbol-infos))
    (list (cons :type :package)
	  (cons :name package-name)
	  (cons :documentation (documentation package t))
	  (cons :external-symbols symbol-infos))))

(provide :swank-info)
