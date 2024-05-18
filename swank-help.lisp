(require :def-properties (merge-pathnames #p"cl-def-properties/module.lisp" (uiop/pathname:pathname-directory-pathname *load-pathname*)))
(ql:quickload "split-sequence")
(require :split-sequence)
(require :uiop)
(require :alexandria)

(defpackage :swank-help
  (:use :cl :def-properties)
  (:shadow #:apropos #:apropos-list #:describe)
  (:export
   :read-emacs-symbol-info
   :read-emacs-package-info
   :read-emacs-system-info
   :read-emacs-packages-info
   :read-emacs-systems-info
   :apropos
   :apropos-list
   :describe)
  (:documentation "Utilities for augmented help."))

(in-package :swank-help)

(defun aget (alist key)
  (cdr (assoc key alist :test 'equalp)))

(defun sort-by-name (infos)
  (sort infos #'string< :key (lambda (info) (aget info :name))))

(defun info-for-emacs (info)
  (when (aget info :package)
    (setf (cdr (assoc :package info))
          (package-name (aget info :package))))
  (when (aget info :arglist)
    ;; arglist is conflictive for slime protocol. do not use.
    (setf (cdr (assoc :arglist info)) nil))
  (when (aget info :documentation)
    (push (cons :parsed-documentation
                (parse-docstring (aget info :documentation)
                                 (when (member (aget info :type) '(:function :generic-function :macro))
                                   (list-lambda-list-args
                                    (read-from-string (aget info :args))))
                                 :package (find-package (aget info :package))))
          info))
  (push (cons :symbol (cdr (assoc :name info))) info)
  (setf (cdr (assoc :name info)) (symbol-name (cdr (assoc :name info))))
  info)

(defun read-emacs-symbol-info (symbol &optional kind shallow)
  (let ((infos (symbol-properties symbol shallow)))
    (if kind
        (alexandria:when-let ((info (find kind infos
                                          :key (lambda (info)
                                                 (aget info :type)))))
          (info-for-emacs info))
        (mapcar 'info-for-emacs infos))))

(defun read-emacs-package-info (package-name &optional shallow)
  (let ((package (or (and (typep package-name 'package)
                          package-name)
                     ;; we use SWANK::PARSE-PACKAGE instead of CL:FIND-PACKAGE here
                     ;; to support alternative values of CL:*PRINT-CASE*
                     (swank::parse-package package-name)
                     (error "Package not found: ~a" package-name))))
    (list (cons :type :package)
          (cons :name (package-name package))
          (cons :documentation (documentation package t))
          (unless shallow
            (let (infos)
              (do-external-symbols (symbol package)
                (dolist (info (read-emacs-symbol-info symbol nil t))
                  (push info infos)))
              (cons :external-symbols (sort-by-name infos)))))))

(defun read-emacs-system-info (system-name &optional shallow)
  (let ((system (asdf:find-system system-name)))
    (list (cons :type :system)
          (cons :name system-name)
          (cons :documentation
                (slot-value system 'asdf/system::description)
                ;;(asdf:system-description system)
                )
          (cons :dependencies (remove-if-not 'stringp
                                             (asdf:system-depends-on system)))
          (cons :loaded-p (asdf:component-loaded-p system-name))
          (unless shallow
            (cons :packages (sort (mapcar 'package-name
                                          (asdf-system-packages system-name))
                                  #'string<))))))

(defun read-emacs-packages-info ()
  (sort-by-name
   (mapcar (lambda (package)
             (read-emacs-package-info package t))
           (list-all-packages))))

(defun read-emacs-systems-info ()
  (sort-by-name
   (mapcar (lambda (system)
             (read-emacs-system-info system t))
           (asdf:registered-systems))))

(swank::defslimefun apropos-documentation-for-emacs
    (pattern &optional external-only case-sensitive package)
  "Make an apropos search in docstrings for Emacs.
The result is a list of property lists."
  (let ((package (if package
                     (or (swank::parse-package package)
                         (error "No such package: ~S" package)))))
    ;; The MAPCAN will filter all uninteresting symbols, i.e. those
    ;; who cannot be meaningfully described.
    (mapcan (swank::listify #'swank::briefly-describe-symbol-for-emacs)
            (sort (remove-duplicates
                   (apropos-symbols-documentation pattern
                                                  :external-only external-only
                                                  :case-sensitive case-sensitive
                                                  :package package))
                  #'swank::present-symbol-before-p))))

(defun some-documentation (symbol)
  ;; Trick to disable warnings from DOCUMENTATION function
  ;; DOCUMENTATION function complains on some type of DOC-TYPE arguments signaling  a warning.
  ;; I temporarily rebind *ERROR-OUTPUT* so those warnings don't appear on the REPL output.
  (let ((*error-output* (make-string-output-stream)))
    (some (lambda (type)
            (documentation symbol type))
          '(function variable type structure setf t))))

(defun make-apropos-documentation-matcher (pattern case-sensitive)
  (let ((chr= (if case-sensitive #'char= #'char-equal)))
    (lambda (docstring)
      (every (lambda (word)
               (search word docstring :test chr=))
             (if (stringp pattern)
                 (list pattern)
                 pattern)))))

(defun apropos-symbols-documentation (pattern &key external-only case-sensitive package return-docs)
  (let ((packages (or package (remove (find-package :keyword)
                                      (list-all-packages))))
        (matcher (make-apropos-documentation-matcher pattern case-sensitive))
        (result))
    (with-package-iterator (next packages :external :internal)
      (loop (multiple-value-bind (morep symbol) (next)
              (when (not morep) (return))
              (let ((doc (some-documentation symbol)))
                (when (or (not external-only) (swank::symbol-external-p symbol))
                  (let ((name-and-doc (if doc
                                          (format nil "~a~%~a" symbol doc)
                                          (symbol-name symbol))))
                    (when (funcall matcher name-and-doc)
                      (if return-docs
                          (push (cons symbol doc) result)
                          (push symbol result)))))))))
    result))

;; Provide an apropos functions that matches on name and docstrings

(defun parse-apropos-pattern (string)
  (split-sequence:split-sequence #\space string :remove-empty-subseqs t))

(defun docstring-summary (docstring)
  (let ((first-line (with-input-from-string (s docstring)
                      (uiop/stream:slurp-stream-line s))))
    (let ((dotpos (position #\. first-line)))
      (if dotpos
          (subseq first-line 0 (1+ dotpos))
          first-line))))

(defun apropos (string-designator &optional package external-only (print-docstring t))
  "Briefly describe all symbols which contain the specified STRING.
If PACKAGE is supplied then only describe symbols present in
that package. If EXTERNAL-ONLY then only describe
external symbols in the specified package.)
If PRINT-DOCSTRING the the results docstrings are made part of the output."
  (let ((packages (or package (remove (find-package :keyword)
                                      (list-all-packages))))
        (matcher (make-apropos-documentation-matcher
                  (parse-apropos-pattern string-designator) nil)))
    (with-package-iterator (next packages :external :internal)
      (loop (multiple-value-bind (morep symbol) (next)
              (when (not morep) (return))
              (let ((doc (some-documentation symbol)))
                (when (or (not external-only) (swank::symbol-external-p symbol))
                  (let ((name-and-doc (if doc
                                          (format nil "~a~%~a" symbol doc)
                                          (symbol-name symbol))))
                    (when (funcall matcher name-and-doc)
                      (if (and print-docstring doc)
                          (format t "~a : ~a" symbol (docstring-summary doc))
                          (format t "~a" symbol))
                      (terpri))))))))
    (values)))

(defun apropos-list (string-designator &optional package external-only)
  "Like SWANK-HELP:APROPOS, except that it returns a list of the symbols found instead of describing them."
  (let ((packages (or package (remove (find-package :keyword)
                                      (list-all-packages))))
        (matcher (make-apropos-documentation-matcher
                  (parse-apropos-pattern string-designator) nil))
        (result))
    (with-package-iterator (next packages :external :internal)
      (loop (multiple-value-bind (morep symbol) (next)
              (when (not morep) (return))
              (let ((doc (some-documentation symbol)))
                (when (or (not external-only) (swank::symbol-external-p symbol))
                  (let ((name-and-doc (if doc
                                          (format nil "~a~%~a" symbol doc)
                                          (symbol-name symbol))))
                    (when (funcall matcher name-and-doc)
                      (pushnew symbol result))))))))
    result))

(defgeneric describe (object &optional destination)
  (:documentation "Print an augmented description of OBJECT to output DESTINATION."))

(defmethod describe (object &optional (destination *standard-output*))
  (cl:describe object destination))

(defmethod describe ((package package) &optional (destination *standard-output*))
  (uiop:with-output (out destination)
    (cl:describe package out)

    (format out "~%Exported definitions: ~%")

    (do-external-symbols (symbol package)
      (let* ((properties (car (symbol-properties symbol t 'cl:macro-function)))
             (args (cdr (assoc :args properties))))
        (when properties
          (when (or (string= args "nil") ;; TODO: fix def-properties
                    (string= args "NIL")
                    (null args))
            (setf args "()"))
          ;;(prin1 properties out)
          (format out "[MACRO] ~a ~a: ~%" symbol args)
          (terpri out)
          (alexandria:if-let ((docs (cdr (assoc :documentation properties))))
            (write-string docs out)
            (write-string "Not documented." out))
          (terpri out)
          (terpri out))))

    (terpri out)

    (do-external-symbols (symbol package)
      (let* ((properties (car (symbol-properties symbol t 'cl:function)))
             (args (cdr (assoc :args properties))))
        (when properties
          (when (or (string= args "nil") ;; TODO: fix def-properties
                    (string= args "NIL")
                    (null args))
            (setf args "()"))
          ;;(prin1 properties out)
          (format out "[FUNCTION] ~a ~a: ~%" symbol args)
          (terpri out)
          (alexandria:if-let ((docs (cdr (assoc :documentation properties))))
            (write-string docs out)
            (write-string "Not documented." out))
          (terpri out)
          (terpri out))))))

;; (describe (find-package :swank-help))


(provide :swank-help)
