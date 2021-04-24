(require :alexandria)
(require :swank)
(require :closer-mop)

(defpackage :def-properties
  (:use :cl)
  (:export
   :symbol-properties
   :function-properties
   :macro-properties
   :class-properties
   :type-properties
   :package-properties
   :parse-docstring
   :list-lambda-list-args
   :asdf-system-packages
   
   :symbol-kinds
   :symbol-kind-p
   :symbol-variable-p
   :symbol-function-p
   
   :symbol-generic-function-p
   :symbol-type-p
   :symbol-class-p
   :symbol-structure-p)
  (:documentation "Collects properties about Lisp definitions, in a portable way"))

(in-package :def-properties)

;; TODO: support all aspects from swank::describe-symbol-for-emacs:
;; :VARIABLE :FUNCTION :SETF :SPECIAL-OPERATOR :MACRO :COMPILER-MACRO
;; :TYPE :CLASS :ALIEN-TYPE :ALIEN-STRUCT :ALIEN-UNION :ALIEN-ENUM

(defun symbol-kinds (symbol)
  "Return the kinds of the SYMBOL."
  (remove-if-not 'symbolp (swank::describe-symbol-for-emacs symbol)))

(defun symbol-kind-p (symbol kind)
  (find kind (symbol-kinds symbol)))

(defun symbol-variable-p (symbol)
  (symbol-kind-p symbol :variable))

(defun symbol-function-p (symbol)
  (symbol-kind-p symbol :function))

(defun symbol-macro-p (symbol)
  (symbol-kind-p symbol :macro))

(defun symbol-generic-function-p (symbol)
  (symbol-kind-p symbol :generic-function))

(defun symbol-type-p (symbol)
  (symbol-kind-p symbol :type))

(defun symbol-class-p (symbol)
  (find-class symbol nil))

(defun symbol-structure-p (symbol)
  (and (find-class symbol nil)
       (typep (find-class symbol nil) 'structure-class)))

(defun symbol-properties (symbol &optional shallow)
  "Collects properties about a symbol.
If TYPE is specified, then SYMBOL is treated as the given TYPE (variable, function, package, etc)."
  (let (properties)
    (when (symbol-function-p symbol)
      (push (function-properties symbol shallow) properties))
    (when (symbol-generic-function-p symbol)
      (push (function-properties symbol shallow) properties))
    (when (symbol-macro-p symbol)
      (push (function-properties symbol shallow) properties))
    (when (symbol-variable-p symbol)
      (push (variable-properties symbol shallow) properties))
    (when (symbol-class-p symbol)
      (push (class-properties symbol shallow) properties))
    ;; TODO
    #+nil(when (symbol-type-p symbol)
      (push (type-properties symbol shallow) properties))
    properties))

(defun aget (alist key)
  (cdr (assoc key alist :test 'equalp)))

(defun package-properties (&optional (package *package*))
  (let (docs)
    (do-external-symbols (symbol package)
      (alexandria:when-let ((symbol-properties (symbol-properties symbol)))
        (push symbol-properties docs)))
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

(defun type-properties (symbol)
  (list (cons :name symbol)
        (cons :package (symbol-package symbol))
        (cons :type :type)
        (cons :documentation (documentation symbol 'type))))

(defun function-properties (symbol &optional shallow)
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase)
                          (*package* (symbol-package symbol)))
                      #+nil(format nil "~{~a~^ ~}"
                                   (mapcar #'format-argument-to-string (swank-backend:arglist symbol))
                                   )
                      (princ-to-string (swank-backend:arglist symbol))))
        (cons :arglist (swank::arglist symbol))
        (cons :package (symbol-package symbol))
        (cons :type (cond ((macro-function symbol) :macro)
                          ((typep (symbol-function symbol) 'generic-function) :generic-function)
                          (t :function)))))

(defun generic-function-properties (symbol &optional shallow)
  (assert (typep (symbol-function symbol) 'generic-function))
  (list (cons :name symbol)
        (cons :documentation (documentation symbol 'function))
        (cons :args (let ((*print-case* :downcase)
                          (*package* (symbol-package symbol)))
                      #+nil(format nil "~{~a~^ ~}"
                                   (mapcar #'format-argument-to-string (swank-backend:arglist symbol))
                                   )
                      (princ-to-string (swank-backend:arglist symbol))))
        (cons :arglist (swank::arglist symbol))
        (cons :package (symbol-package symbol))
        (cons :type :generic-function)
	(unless shallow
	  (cons :methods (closer-mop:generic-function-methods (symbol-function symbol))))))

(defun variable-properties (symbol &optional shallow)
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

(defun assoc-name (v)
  (assoc-cdr :name v :error-p t))

(defun specialise->symbol (spec)
  (case (caar spec)
    ((defmethod) (cadar spec))
    #+ccl((ccl::reader-method) (cadr (assoc :method (cdar spec))))
    (t nil)))

(defun symbol-external-p (symbol &optional (package (symbol-package symbol)))
  "Return non-NIL if SYMBOL is external in PACKAGE. SYMBOL may be either
a symbol, or a SETF form, in which case the check will be performed on
the CADR of the list."
  (eq (nth-value
       1
       (find-symbol
        (symbol-name
         (cond ((symbolp symbol)
                symbol)
               ((eq (car symbol) 'setf)
                (cadr symbol))
               (t (error "Unknown symbol type: ~s" symbol))))
        package))
      :external))

(defun specialisation-properties (class-name)
  (let* ((ignored '(initialize-instance))
         (class (if (symbolp class-name) (find-class class-name) class-name))
         (spec (swank-backend:who-specializes class)))
    (unless (eq spec :not-implemented)
      (sort (loop
              for v in spec
              for symbol = (specialise->symbol v)
              when (and (not (member symbol ignored))
                        (symbol-external-p symbol (symbol-package (class-name class))))
                collect (list (cons :name symbol) (cons :documentation (documentation symbol 'function))))
            #'string< :key (alexandria:compose #'princ-to-string #'assoc-name)))))

(defun %ensure-external (symbol)
  (let ((name (cond ((symbolp symbol)
                     symbol)
                    ((and (listp symbol) (eq (car symbol) 'setf))
                     (cadr symbol))
                    (t
                     (warn "Unknown type: ~s. Expected symbol or SETF form." symbol)
                     nil))))
    (when (swank::symbol-external-p name)
      symbol)))

(defun accessor-properties (class slot)
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
                 (cons :accessors (let ((accessor-list (accessor-properties class slot)))
                                    (when accessor-list
                                      (list accessor-list)))))))
    (mapcar #'load-slot (closer-mop:class-slots class))))

(defun class-properties (class-name &optional shallow)
  (let ((cl (find-class class-name)))
    (list (cons :name          (class-name cl))
          (cons :documentation (documentation cl 'type))
          (unless shallow (cons :slots (load-slots cl)))
          (unless shallow (cons :methods (specialisation-properties cl)))
          (unless shallow (cons :class-precedence-list (mapcar 'class-name (find-superclasses cl))))
          (unless shallow (cons :direct-superclasses (mapcar 'class-name (closer-mop:class-direct-superclasses cl))))
          (unless shallow (cons :direct-subclasses (mapcar 'class-name (closer-mop:class-direct-subclasses cl))))
          (cons :package (symbol-package class-name))
          (cons :type :class))))

(defun %annotate-function-properties (fn-properties classes)
  "Append :ACCESSORP tag if the function is present as an accessor function."
  (loop
    with name = (cdr (assoc :name fn-properties))
    for class-properties in classes
    do (loop
         for slot-properties in (cdr (assoc :slots class-properties))
         do (loop
              for accessor in (cdr (assoc :accessors slot-properties))
              for accessor-sym = (cdar accessor)
              when (or (and (symbolp accessor-sym) (eq accessor-sym name))
                       (and (listp accessor-sym) (eq (car accessor-sym) 'setf) (eq (cadr accessor-sym) name)))
                do (return-from %annotate-function-properties (append fn-properties '((:accessorp t))))))
    finally (return fn-properties)))

;; docbrowser stuff ends here

(defun concat-rich-text (text)
  (when (stringp text)
    (return-from concat-rich-text text))
  (let ((segments nil)
        (segment nil))
    (loop for word in text
          do (if (stringp word)
                 (push word segment)
                 ;; else, it is an "element"
                 (destructuring-bind (el-type content) word
                   (push (apply #'concatenate 'string (nreverse segment))
                         segments)
                   (setf segment nil)
                   (push (list el-type (concat-rich-text content))
                         segments)))
          finally (when segment
                    (push (apply #'concatenate 'string (nreverse segment))
                          segments)))
    (nreverse segments)))

(defun split-string-with-delimiter (string delimiter
                                    &key (keep-delimiters t)
                                    &aux (l (length string)))
  (let ((predicate (cond
                     ((characterp delimiter) (lambda (char) (eql char delimiter)))
                     ((listp delimiter) (lambda (char) (member char delimiter)))
                     ((functionp delimiter) delimiter)
                     (t (error "Invalid delimiter")))))
    (loop for start = 0 then (1+ pos)
          for pos   = (position-if predicate string :start start)

          ;; no more delimiter found
          when (and (null pos) (not (= start l)))
            collect (subseq string start)

          ;; while delimiter found
          while pos

          ;;  some content found
          when (> pos start) collect (subseq string start pos)
            ;;  optionally keep delimiter
            when keep-delimiters collect (string (aref string pos)))))

(defun list-lambda-list-args (lambda-list)
  "Takes a LAMBDA-LIST and returns the list of all the argument names."
  (loop for symbol in (alexandria:flatten lambda-list)
        when (and (symbolp symbol)
		  (not (char-equal (aref (symbol-name symbol) 0) #\&))) ;; special argument
          collect symbol))

;; (list-lambda-list-args '(foo))
;; (list-lambda-list-args '(foo &optional bar))
;; (list-lambda-list-args '(foo &optional (bar 22)))
;; (list-lambda-list-args '(foo &optional (bar 22) &key key (key2 33) &rest args &body body))
;; (list-lambda-list-args '((stream-name file-name &rest args &key (direction) &allow-other-keys) &body body))

(defun parse-docstring (docstring bound-args &key case-sensitive (package *package*))
  "Parse a docstring.
BOUND-ARGS: when parsing a function/macro/generic function docstring, BOUND-ARGS contains the names of the arguments. That means the function arguments are detected by the parser.
CASE-SENSITIVE: when case-sensitive is T, bound arguments are only parsed when in uppercase.
"
  (let ((words (split-string-with-delimiter
                docstring
                (lambda (char)
                  (not
                   (or (alphanumericp char)
                       (find char "+-*/@$%^&_=<>~:"))))))
        (string-test (if case-sensitive
                         'string=
                         'equalp)))
    (concat-rich-text
     (loop for word in words
           collect (cond
                     ((member (string-upcase word) (mapcar 'symbol-name bound-args) :test string-test)
                      (list :arg word))
                     ((fboundp (intern word package))
                      (list :fn word))
                     ((boundp (intern word package))
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

(defun location-pathname (location)
  (pathname
   (cadr
    (or (find :file (cdr location)
              :key 'car)
        (find :buffer-and-file (cdr location)
              :key 'car)
        ))))

(defvar *package-source-locations* (make-hash-table)
  "A cache of packages source locations")

(defun package-source-location (package)
  (or (gethash package *package-source-locations*)
      (setf (gethash package *package-source-locations*)
            (swank/backend:find-source-location package))))

;; This function finds the packages defined from an ASDF, approximatly. And it is very slow.
(defun asdf-system-packages (system)
  (when (not (asdf:component-loaded-p system))
    (return-from asdf-system-packages nil))
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

(provide :def-properties)
