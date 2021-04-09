;;; slime-info --- Slime info
;;; Commentary:
;;; This is a package

;;; Code:

(require 'slime)

(defun slime-info/display-info-buffer (texinfo-source)
  (let ((temp-file (make-temp-file "info-buffer-test")))
    (with-temp-file temp-file
      (insert texinfo-source))
    (let ((buffer (find-file-noselect temp-file)))
      (with-current-buffer buffer
        (makeinfo-buffer)
        (display-buffer)))))

(defun slime-info-symbol (symbol-name)
  (interactive (list (slime-read-symbol-name "Symbol: ")))
  (when (not symbol-name)
    (error "No symbol name given"))
  (let ((texinfo-source (slime-eval `(swank:texinfo-source-for-symbol ,symbol-name))))
    (slime-info/display-info-buffer texinfo-source)))

(defun slime-info-package (package-name)
  "Show information about Common Lisp package named PACKAGE-NAME, using an Info buffer."
  (interactive (list (slime-read-package-name "Package name: ")))
  (when (not package-name)
    (error "No package name given"))
  (let ((texinfo-source (slime-eval `(swank:texinfo-source-for-package ,package-name))))
    (slime-info/display-info-buffer texinfo-source)))

(defun slime-info-apropos (symbol-name)
  (interactive (list (slime-read-symbol-name "Apropos symbol info: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (if (position 58 symbol-name) ;; 58 is the colon character
      (info-apropos symbol-name)
    (let* ((symbol-package-name
            (slime-eval
             `(cl:package-name
               (cl:symbol-package (cl:read-from-string ,(concat (remove 58 (slime-current-package)) "::" symbol-name))))))
           (index-entry (concat symbol-package-name ":" symbol-name)))
      (info-apropos index-entry))))

(define-slime-contrib slime-info
  "Online Common Lisp documentation via Emacs Info."
  (:authors "Mariano Montone <marianomontone@gmail.com>")
  (:license "GPL")
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-info))

;;; Utilities

(defgroup slime-info nil
  "Quicklisp support for Slime."
  :prefix "slime-info-"
  :group 'slime)


(provide 'slime-info)
;;; slime-info.el ends here
