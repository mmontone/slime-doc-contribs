(require :clhs)
(require :alexandria)
(require :hyperspec-lookup)

(defpackage :clhs-docstrings
  (:use :cl)
  (:export :create-file-with-docstrings))

(in-package :clhs-docstrings)

(setf hyperspec::*hyperspec-root* (princ-to-string (clhs:hyperspec-root)))

(defun create-file-with-docstrings (file)
  (with-open-file (f file :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (do-external-symbols (symbol :cl)
      (let ((hyperspec-file (hyperspec:lookup (symbol-name symbol))))
	(when (and hyperspec-file (probe-file hyperspec-file))
	  (let ((hyperspec-text
		  (with-output-to-string (s)
		    (uiop:run-program (format nil "html2text -style pretty ~a" hyperspec-file)
				      :output s))))
	    (write `(setf (documentation ',symbol t)
			  ,(concatenate 'string
				       (or (documentation symbol t) "")
				       hyperspec-text))
		   :stream f)
	    (terpri f)))))))

;; (create-file-with-docstrings "/mnt/sdb2/home/marian/src/lisp/slime-star/slime-doc-contribs/clhs-docstrings.gen.lisp")
