(require :clhs)
(require :alexandria)
(require :hyperspec-lookup)
(require :str)

(defpackage :clhs-docstrings
  (:use :cl)
  (:export :create-file-with-docstrings))

(in-package :clhs-docstrings)

(setf hyperspec::*hyperspec-root* (princ-to-string (clhs:hyperspec-root)))

(defmacro condp (predicate &body clauses)
  "COND using PREDICATE."
  (let ((pred (gensym)))
    `(let ((,pred ,predicate))
       (cond
         ,@(loop for clause in clauses
                 collect `((funcall ,pred ,(car clause))
                           ,@(cdr clause)))))))

(defun create-file-with-docstrings (file &optional (method :append))
  (check-type method (member :append :replace))
  (with-open-file (f file :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (do-external-symbols (symbol :cl)
      (let ((hyperspec-file (hyperspec:lookup (symbol-name symbol))))
        (when (and hyperspec-file (probe-file hyperspec-file))
          (let ((hyperspec-text
		  (string-trim '(#\space #\newline #\tab)
			       (with-output-to-string (s)
				 (uiop:run-program (format nil "html2text -style pretty ~a" hyperspec-file)
						   :output s))))
                (doc-type (condp (lambda (x) (str:starts-with-p x (pathname-name hyperspec-file)))
                            ("v_" 'variable)
                            ("f_" 'function)
                            ("m_" 'function)
                            ("t_" 'type)
                            ("a_" nil) ;; ignore for now
                            ("d_" nil) ;; ignore for now
                            ("e_" nil)
                            (""
                             ;;(error "Unrecognized: ~a" (pathname-name hyperspec-file))
                             nil
                             ))))
            (format t ".")
            (when doc-type
              (let ((docstring (documentation symbol doc-type)))
                (write `(setf (documentation ',symbol ',doc-type)
                              ,(if (and docstring (eql method :append))
                                   (format nil "~a~%~%~a" (string-trim '(#\space #\newline #\tab) docstring) hyperspec-text)
                                   hyperspec-text))
                       :stream f))
              (terpri f)(terpri f))))))))

;; (create-file-with-docstrings "/mnt/sdb2/home/marian/src/lisp/slime-star/slime-doc-contribs/clhs-docstrings.gen.lisp")

;; (load "/mnt/sdb2/home/marian/src/lisp/slime-star/slime-doc-contribs/clhs-docstrings.gen.lisp")
