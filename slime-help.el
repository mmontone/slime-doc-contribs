;; -*- lexical-binding: t -*-

(require 'cl)
(require 'anaphora)
(require 'map)
(require 'button)
(require 'dash)
(require 'lisp-mode)
(require 'slime)

(defface slime-help-heading-1
  '((t :weight bold
       :height 1.5
       ;;:underline t
       ))
  "Slime help face for headings"
  :group 'slime-help-faces)

(defface slime-help-heading-2
  '((t :weight bold
       :height 1.3
       ;;:underline t
       ))
  "Slime help face for headings"
  :group 'slime-help-faces)

(defface slime-help-heading-3
  '((t :weight bold
       :height 1.1
       ;;:underline t
       ))
  "Slime help face for headings"
  :group 'slime-help-faces)

(defface slime-help-argument
  '((t :foreground "darkorange"))
  "Face for variables in Slime help"
  :group 'slime-help-faces)

(defface slime-help-variable
  '((t :foreground "darkgreen"))
  "Face for variables in Slime help"
  :group 'slime-help-faces)

(defface slime-help-name
  '((t :foreground "orange"))
  "Face for name in Slime help"
  :group 'slime-help-faces)

(defface slime-help-type
  '((t :foreground "purple"))
  "Face for type in Slime help"
  :group 'slime-help-faces)

(defun slime-help--heading-1 (text)
  (propertize text 'face 'slime-help-heading-1))

(defun slime-help--heading-2 (text)
  (propertize text 'face 'slime-help-heading-2))

(defun slime-help--heading-3 (text)
  (propertize text 'face 'slime-help-heading-3))

(defun slime-help--horizontal-line (&rest width)
  (make-string (or width 80) ?\u2500))


(defun render-parsed-docstring (docstring)
  (dolist (word docstring)
    (cond
     ((stringp word) (insert word))
     ((and (listp word) (eql (first word) :arg))
      (insert (propertize (second word) 'face 'slime-help-argument)))
     ((and (listp word) (eql (first word) :fn))
      (insert-button (second word)
		     'action (lambda (btn)
			       (slime-help-symbol (second word)))
		     'follow-link t
		     'help-echo "Describe function"))
     ((and (listp word) (eql (first word) :key))
      (insert (propertize (second word) 'face 'slime-help-keyword)))
     ((and (listp word) (eql (first word) :var))
      (insert (propertize (second word) 'face 'slime-help-variable)))
     (t (error "Don't know how to render")))))

(defun slime-help-symbol (symbol-name)
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((symbol-info (slime-eval `(swank-help:read-emacs-symbol-info (cl:read-from-string ,(slime-qualify-cl-symbol-name symbol-name))))))
    (case (cdr (assoc :type symbol-info))
      (:function (slime-help-function symbol-name))
      (:package (slime-help-package symbol-name))
      (t (error "TODO")))))

;;(slime-help-symbol "ALEXANDRIA:FLATTEN")

(defun slime-help--kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun slime-help-package (package-name)
  (interactive (list (slime-read-package-name "Describe package: ")))
  (when (not package-name)
    (error "No package name given"))

  (let ((buffer-name (format "*slime-help: %s package*" package-name)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (return-from slime-help-package))
    (let* ((package-info (slime-eval `(swank-help:read-emacs-package-info ,package-name)))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (insert (slime-help--heading-1 (upcase (string-trim package-name))))
        (newline 2)
        (insert (format "This is a Common Lisp package with %d external symbols" (length (cdr (assoc :external-symbols package-info)))))
        (newline 2)
        (when (cdr (assoc :documentation package-info))
          (insert (cdr (assoc :documentation package-info)))
          (newline 2))

        (cl-flet ((goto-source (btn)
                               (slime-edit-definition-other-window package-name)))
          (insert-button "Source"
                         'action (function goto-source)
                         'follow-link t
                         'help-echo "Go to package source code"))
        (newline 2)

        (insert (slime-help--heading-2 "Exported symbols"))
        (newline 2)
        (insert (slime-help--horizontal-line))
	(newline)
        (dolist (symbol-info (cdr (assoc :external-symbols package-info)))
          (insert (propertize (subseq (symbol-name (cdr (assoc :type symbol-info))) 1) 'face 'slime-help-type))
          (insert " ")
          (insert-button (format "%s" (cdr (assoc :name symbol-info)))
                         'action (lambda (btn)
                                   (slime-help-symbol (prin1-to-string (cdr (assoc :symbol symbol-info)))))
                         'follow-link t
                         'help-echo "Describe symbol")
          (newline)
          (if (cdr (assoc :documentation symbol-info))
              ;;(insert (cdr (assoc :documentation symbol-info)))
              (render-parsed-docstring (cdr (assoc :parsed-documentation symbol-info)))
            (insert "Not documented"))
          (newline)
          (insert (slime-help--horizontal-line))
          (newline))
        (setq buffer-read-only t)
        (local-set-key "q" 'slime-help--kill-current-buffer)
	(local-set-key "Q" 'slime-help--kill-all-help-buffers)
        (buffer-disable-undo)
        (set (make-local-variable 'kill-buffer-query-functions) nil)
        (goto-char 0)
        (pop-to-buffer buffer)
        nil))))

;;(slime-help-package "ALEXANDRIA")

(defun slime-help-function (symbol-name)
  (interactive (list (slime-read-symbol-name "Describe symbol's function: ")))
  (when (not symbol-name)
    (error "No symbol given"))

  (let ((buffer-name (format "*slime-help: %s function*" symbol-name)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (return-from slime-help-function))

    (let* ((symbol-info (slime-eval `(swank-help:read-emacs-symbol-info (cl:read-from-string ,(slime-qualify-cl-symbol-name symbol-name)))))
           (package-name (cdr (assoc :package symbol-info)))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (insert (slime-help--heading-1 (cdr (assoc :name symbol-info))))
        (newline 2)
        (insert (format "This is a FUNCTION in package "))
        (insert-button package-name
                       'action (lambda (btn)
                                 (slime-help-package package-name))
                       'follow-link t
                       'help-echo "Describe package")
        (newline 2)
        (insert (slime-help--heading-3 "Signature"))
        (newline)
        (insert (--highlight-syntax (cdr (assoc :args symbol-info))))
        (newline 2)
        (render-parsed-docstring (cdr (assoc :parsed-documentation symbol-info)))
        (newline 2)
        (cl-flet ((goto-source (btn)
                               (slime-edit-definition-other-window (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "Source"
                         'action (function goto-source)
                         'follow-link t
                         'help-echo "Go to definition source code"))
        (insert " ")
        (cl-flet ((browse-references (btn)
                                     (slime-who-calls (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "References"
                         'action (function browse-references)
                         'follow-link t
                         'help-echo "Browse references"))
        (insert " ")
        (cl-flet ((disassemble-function (btn)
                                        (slime-disassemble-symbol (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "Disassemble"
                         'action (function disassemble-function)
                         'follow-link t
                         'help-echo "Disassemble function"))
        (insert " ")
        (cl-flet ((lookup-in-info (btn)
                                  (info-apropos (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "Lookup in manual"
                         'action (function lookup-in-info)
                         'help-echo "Search for this in Info manuals"
                         'follow-link t))
        (setq buffer-read-only t)
        (local-set-key "q" 'slime-help--kill-current-buffer)
	(local-set-key "Q" 'slime-help--kill-all-help-buffers)
        (buffer-disable-undo)
        (set (make-local-variable 'kill-buffer-query-functions) nil)
        (goto-char 0)
        (pop-to-buffer buffer)
        nil))))

;;(slime-help-function "ALEXANDRIA:FLATTEN")
;;(slime-help-function "SPLIT-SEQUENCE:SPLIT-SEQUENCE")

;; This was copied from help.el
(defun --highlight-syntax (source &optional mode)
  "Return a propertized version of SOURCE in MODE."
  (unless mode
    (setq mode #'lisp-mode))
  (if (or
       (< (length source) 5000)
       (eq mode 'emacs-lisp-mode))
      (with-temp-buffer
        (insert source)

        ;; Switch to major-mode MODE, but don't run any hooks.
        (delay-mode-hooks (funcall mode))

        ;; `delayed-mode-hooks' contains mode hooks like
        ;; `emacs-lisp-mode-hook'. Build a list of functions that are run
        ;; when the mode hooks run.
        (let (hook-funcs)
          (dolist (hook delayed-mode-hooks)
            (let ((funcs (symbol-value hook)))
              (setq hook-funcs (append hook-funcs funcs))))

          ;; Filter hooks to those that relate to highlighting, and run them.
          ;;(setq hook-funcs (-intersection hook-funcs help--highlighting-funcs))
          ;;(-map #'funcall hook-funcs)
          )

        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))
        (buffer-string))
    ;; SOURCE was too long to highlight in a reasonable amount of
    ;; time.
    source))

(defun slime-help-system (system-name)

  (interactive (list (slime-read-system-name "Describe system")))
  (when (not system-name)
    (error "No system name given"))

  (let ((buffer-name (format "*slime-help: %s system*" system-name)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (return-from slime-help-system))

    (let* ((system-info (slime-eval `(swank-help:read-emacs-system-info ,system-name)))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (insert (slime-help--heading-1 (upcase system-name)))
        (newline 2)
        (insert (format "This is a Common Lisp ASDF system with %d dependencies" (length (cdr (assoc :dependencies system-info)))))
        (newline 2)
        (when (cdr (assoc :documentation system-info))
          (insert (cdr (assoc :documentation system-info)))
          (newline 2))
        (insert (slime-help--heading-2 "Dependencies"))
        (newline 2)
        (if (zerop (length (cdr (assoc :dependencies system-info))))
            (insert "It has no dependencies")
          ;; else
          (dolist (dependency (cdr (assoc :dependencies system-info)))
            (insert "* ")
            (insert-button dependency
                           'action (lambda (btn)
                                     (slime-help-system dependency))
                           'follow-link t
                           'help-echo "Describe system")
            (newline)))

        (newline 2)

        (cl-flet ((open-system (btn)
                               (slime-open-system system-name)))
          (insert-button "Open"
                         'action (function open-system)
                         'follow-link t
                         'help-echo "Open system"))

        (insert " ")

        (cl-flet ((browse-system (btn)
                                 (slime-browse-system system-name)))
          (insert-button "Browse"
                         'action (function browse-system)
                         'follow-link t
                         'help-echo "Browse system"))

        (insert " ")

        (cl-flet ((load-system (btn)
                               (slime-load-system system-name)))
          (insert-button "Load"
                         'action (function load-system)
                         'follow-link t
                         'help-echo "Load system"))

	(newline)

	(setq buffer-read-only t)
	(local-set-key "q" 'slime-help--kill-current-buffer)
	(buffer-disable-undo)
	(set (make-local-variable 'kill-buffer-query-functions) nil)
	(goto-char 0)
	(pop-to-buffer buffer)
	nil))))

;;(slime-help-system "alexandria")

(defun slime-help-apropos ()
  (debug "TODO"))

(defun slime-help-apropos-all ()
  (debug "TODOE"))

(defun slime-help-setup-key-bindings ()
  (define-key slime-doc-map "a" 'slime-help-apropos)
  (define-key slime-doc-map "z" 'slime-help-apropos-all)
  (define-key slime-doc-map "d" 'slime-help-symbol)
  (define-key slime-doc-map "f" 'slime-help-function)
  (define-key slime-doc-map "p" 'slime-help-package)
  (define-key slime-doc-map "s" 'slime-help-system))

(define-slime-contrib slime-help
  "Augmented help"
  (:authors "Mariano Montone")
  (:license "GPL")
  (:slime-dependencies slime-asdf)
  (:swank-dependencies swank-help)
  (:on-load
   ;; setup key bindings
   (slime-help-setup-key-bindings)))

(defun slime-help-init ()
  (slime-help-setup-key-bindings))


(provide 'slime-help)
