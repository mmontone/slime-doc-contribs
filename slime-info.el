;;; slime-info --- Slime info
;;; Commentary:
;;; This is a package

;;; Code:

(require 'slime)
(require 'slime-asdf)

(defun slime-info/display-info-buffer (texinfo-source)
  (let ((temp-file (make-temp-file "info-buffer-")))
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

(defun slime-info-system (system-name)
  "Show information about Common Lisp ASDF system named SYSTEM-NAME, using an Info buffer."
  (interactive (list (slime-read-system-name "System name: ")))
  (when (not system-name)
    (error "No ASDF system name given"))
  (let ((texinfo-source (slime-eval `(swank:texinfo-source-for-system ,system-name))))
    (slime-info/display-info-buffer texinfo-source)))

;; (defun slime-info-apropos (symbol-name)
;;   (interactive (list (slime-read-symbol-name "Apropos symbol info: ")))
;;   (when (not symbol-name)
;;     (error "No symbol given"))
;;   (if (position 58 symbol-name) ;; 58 is the colon character
;;       (info-apropos symbol-name)
;;     (let* ((symbol-package-name
;;             (slime-eval
;;              `(cl:package-name
;;                (cl:symbol-package (cl:read-from-string ,(concat (remove 58 (slime-current-package)) "::" symbol-name))))))
;;            (index-entry (concat symbol-package-name ":" symbol-name)))
;;       (info-apropos index-entry))))

(defun slime-info-apropos (string &optional only-external-p package
                             case-sensitive-p)
  "Show all bound symbols whose names match STRING. With prefix
arg, you're interactively asked for parameters of the search."
  (interactive
   (if current-prefix-arg
       (list (read-string "SLIME Apropos: ")
             (y-or-n-p "External symbols only? ")
             (let ((pkg (slime-read-package-name "Package: ")))
               (if (string= pkg "") nil pkg))
             (y-or-n-p "Case-sensitive? "))
     (list (read-string "SLIME Apropos: ") t nil nil)))
  (let ((buffer-package (or package (slime-current-package))))
    (let ((texinfo-source (slime-eval `(swank:texinfo-source-for-apropos ,string ,only-external-p ,case-sensitive-p ',package))))
      (slime-info/display-info-buffer texinfo-source))))

(defun slime-info-apropos-all ()
  "Shortcut for (slime-apropos <string> nil nil)"
  (interactive)
  (slime-info-apropos (read-string "SLIME Apropos: ") nil nil))

(defun slime-info-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (slime-read-package-name "Package: ")))
                       (if (string= pkg "") (slime-current-package) pkg))
                     current-prefix-arg))
  (slime-apropos "" (not internal) package))

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
