(require :def-properties (merge-pathnames #p"cl-def-properties/module.lisp" (uiop/pathname:pathname-directory-pathname *load-pathname*)))

(defpackage :swank-info
  (:use :cl :swank :def-properties)
  (:export
   :texinfo-source-for-function
   :texinfo-source-for-package
   :texinfo-source-for-system
   :texinfo-source-for-apropos
   :texinfo-source-for-symbol))

(in-package :swank-info)

(defun aget (alist key)
  (cdr (assoc key alist :test 'equalp)))

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
           (package-info (package-properties package)))
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
    (let* ((arg-names (list-lambda-list-args (aget info :arglist))))
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

(swank::defslimefun texinfo-source-for-package (package-name)
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
    (let* ((symbol-info (symbol-properties symbol)))
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
             (render-info (symbol-properties symbol) stream :package package)
             (ln))
    (ln)
    (fmt "@bye")))

(swank::defslimefun texinfo-source-for-symbol (symbol-name)
  (with-output-to-string (s)
    (render-texinfo-source-for-symbol (read-from-string symbol-name) s)))

(swank::defslimefun texinfo-source-for-apropos (name &optional external-only
                                                     case-sensitive package)
  "Make an apropos search for Emacs. Show the result in an Info buffer."
  (let ((package (when package
                   (or (swank::parse-package package)
                       (error "No such package: ~S" package)))))
    (let ((symbols (swank::apropos-symbols name external-only case-sensitive package)))
      (with-output-to-string (s)
        (render-texinfo-source-for-symbols
         ;;(format nil "Apropos: ~a" name)
         name
         symbols s
         :package package)))))

(defun render-texinfo-node-for-package (package stream)
  (flet ((fmt (str &rest args)
           (apply #'format stream str args))
         (fmtln (str &rest args)
           (apply #'format stream str args)
           (terpri stream))
         (ln ()
           (terpri stream)))
    (let* ((package-info (package-properties package)))
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
             (setq result (replace-all result "@subsubsection" "@part"))
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
           (system-packages (when (asdf:component-loaded-p asdf-system)
			      (def-properties:asdf-system-packages asdf-system)))
           (readme-file (find "README"
                              (uiop/filesystem:directory-files (asdf:system-source-directory asdf-system))
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

(swank::defslimefun texinfo-source-for-system (system-name &key (use-pandoc t))
  (with-output-to-string (s)
    (render-texinfo-source-for-system
     (asdf:find-system system-name) s
     :use-pandoc use-pandoc)))

(defun render-parsed-docstring (docstring stream)
  (loop for word in docstring
        do
           (cond
             ((stringp word) (write-string word stream))
             ((and (listp word) (eql (car word) :arg))
              (format stream "@var{~a}" (second word)))
             ((and (listp word) (eql (car word) :fn))
	      ;; We would like to do this, but we have to make sure the referenced thing exists
              ;;(format stream "@ref{~a}" (second word))
	      (format stream "@code{~a}" (second word))
	      )
             ((and (listp word) (eql (car word) :var))
	      ;; We would like to do create references, but we have to make sure the referenced thing exists.
	      ;; makeinfo command can be called with --no-validate option for this.
	      ;; in Emacs, customize makeinfo-options variable (add --no-validate option)
              ;;(format stream "@ref{~a}" (second word))
	      (format stream "@var{~a}" (second word))
	      )
             ((and (listp word) (eql (car word) :key))
              (format stream "@var{~a}" (second word))))))

;; (render-parsed-docstring (parse-docstring "lala :lolo" nil) t)
;; (render-parsed-docstring (parse-docstring "funcall parse-docstring" nil) t)
;; (render-parsed-docstring (parse-docstring "asdf" '(asdf)) t)

(provide :swank-info)
