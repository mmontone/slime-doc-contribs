(require :def-properties (merge-pathnames #p"def-properties.lisp" (uiop/pathname:pathname-directory-pathname *load-pathname*)))

(defpackage :swank-help
  (:use :cl :swank :def-properties)
  (:export
   :read-emacs-symbol-info
   :read-emacs-package-info
   :read-emacs-system-info))

(in-package :swank-help)

(defun aget (alist key)
  (cdr (assoc key alist :test 'equalp)))

(defun info-for-emacs (info)
  (when (aget info :package)
    (setf (cdr (assoc :package info))
	  (package-name (aget info :package))))
  (when (aget info :documentation)
    (push (cons :parsed-documentation
		(parse-docstring (aget info :documentation)
				 (when (member (aget info :type) '(:function :generic-function :macro))
				   (list-lambda-list-args
				    (aget info :arglist)))))
	  info))
  (push (cons :symbol (cdr (assoc :name info))) info)
  (setf (cdr (assoc :name info)) (symbol-name (cdr (assoc :name info))))
  info)

(defun read-emacs-symbol-info (symbol &optional kind)
  (let ((infos (symbol-properties symbol)))
    (if kind
	(alexandria:when-let ((info (find kind infos :key (lambda (info)
							    (aget info :type)))))
	  (info-for-emacs info))
	(mapcar 'info-for-emacs infos))))

(defun read-emacs-package-info (package-name)
  (let ((package (or (find-package package-name)
                     (error "Package not found: ~a" package-name)))
        symbol-infos)
    (do-external-symbols (symbol package)
      (alexandria:when-let ((symbol-info (read-emacs-symbol-info symbol)))
	(push symbol-info symbol-infos)))
    (list (cons :type :package)
          (cons :name package-name)
          (cons :documentation (documentation package t))
          (cons :external-symbols (apply #'append symbol-infos)))))

(defun read-emacs-system-info (system-name)
  (let ((system (asdf:find-system system-name)))
    (list (cons :type :system)
          (cons :name system-name)
          (cons :documentation (asdf:system-description system))
          (cons :dependencies (asdf:system-depends-on system))
	  (cons :loaded-p (asdf:component-loaded-p system-name))
	  (cons :packages (mapcar 'package-name (asdf-system-packages system-name))))))

(provide :swank-help)
