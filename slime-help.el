;; -*- lexical-binding: t -*-

(require 'cl)
(require 'anaphora)
(require 'map)
(require 'button)
(require 'dash)
(require 'lisp-mode)
(require 'slime)

(defface slime-help-heading
  '((t :weight bold :underline t))
  "Slime help face for headings"
  :group 'slime-help-faces)

(defface slime-help-variable
  '((t :foreground "orange"))
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

;; (let ((buffer (get-buffer-create "*slime-help*")))
;;   (with-current-buffer buffer
;;     (insert-button
;;      "foo"
;;      'action (lambda (x) (find-file "~/test.py"))
;;      'follow-link t
;;      'help-echo "go")
;;     (pop-to-buffer buffer)))

;;(slime-eval `(swank::read-elisp-symbol-info 'alexandria:flatten))

(defun render-parsed-docstring (docstring)
  (dolist (word docstring)
    (cond
     ((stringp word) (insert word))
     ((and (listp word) (eql (first word) :arg))
      (insert (propertize (second word) 'face 'highlight)))
     ((and (listp word) (eql (first word) :fn))
      (insert (propertize (second word) 'face 'link)))
     ((and (listp word) (eql (first word) :key))
      (insert (propertize (second word) 'face 'underline)))
     ((and (listp word) (eql (first word) :var))
      (insert (propertize (second word) 'face 'highlight)))
     (t (error "Don't know how to render")))))

(defun sh--propertize-heading (text)
  (propertize text 'face 'slime-help-heading))

(defun slime-help-symbol (symbol-name)
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((symbol-info (slime-eval `(swank::read-elisp-symbol-info (swank::read-from-string ,symbol-name)))))
    (case (cdr (assoc :type symbol-info))
      (:function (slime-help-function symbol-name))
      (:package (slime-help-package symbol-name))
      (t (error "TODO")))))

;;(slime-help-symbol "ALEXANDRIA:FLATTEN")

(defun kill-current-buffer ()
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
    (let* ((package-info (slime-eval `(swank::read-elisp-package-info ,package-name)))
	   (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
	(insert (sh--propertize-heading (upcase package-name)))
	(newline 2)
	(insert (format "This is a Common Lisp package with %d external symbols" (length (cdr (assoc :external-symbols package-info)))))
	(newline 2)
	(when (cdr (assoc :documentation package-info))
          (insert (cdr (assoc :documentation package-info)))
	  (newline 2))      
	(insert (sh--propertize-heading "Exported symbols"))
	(newline 2)
	(insert (make-string 80 ?\u2500))
	(dolist (symbol-info (cdr (assoc :external-symbols package-info)))
	  (insert (propertize (prin1-to-string (cdr (assoc :type symbol-info))) 'face 'slime-help-type))
          (insert " ")
	  (insert-button (princ (cdr (assoc :name symbol-info)))
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
	  (insert (make-string 80 ?\u2500))
	  (newline))
	(setq buffer-read-only t)
	(local-set-key "q" 'kill-current-buffer)
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
    
    (let* ((symbol-info (slime-eval `(swank::read-elisp-symbol-info (swank::read-from-string ,symbol-name))))
	   (package-name (cdr (assoc :package symbol-info)))
	   (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
	(insert (sh--propertize-heading (cdr (assoc :name symbol-info))))
	(newline 2)
	(insert (format "This is a FUNCTION in package "))
	(insert-button package-name
                       'action (lambda (btn)
				 (slime-help-package package-name))
		       'follow-link t
		       'help-echo "Describe package")
	(newline 2)
	(insert (sh--propertize-heading "Signature"))
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
	(local-set-key "q" 'kill-current-buffer)
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
