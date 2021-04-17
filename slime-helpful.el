(setq lexical-binding t)

(require 'cl)
(require 'anaphora)
(require 'map)

(defface slime-helpful-heading
  '((t :weight bold :underline t))
  "Slime helpful face for headings"
  :group 'slime-helpful-faces)

(defface slime-helpful-variable
  '((t :foreground "orange"))
  "Face for variables in Slime helpful"
  :group 'slime-helpful-faces)

(defface slime-helpful-name
  '((t :foreground "orange"))
  "Face for name in Slime helpful"
  :group 'slime-helpful-faces)

(defface slime-helpful-type
  '((t :foreground "purple"))
  "Face for type in Slime helpful"
  :group 'slime-helpful-faces)

(let ((buffer (get-buffer-create "*slime-helpful*")))
  (with-current-buffer buffer
    (insert "hello")
    (insert "\n")
    (insert (propertize "Function" 'face 'slime-helpful-heading))
    (insert "\n")
    (insert (propertize "Italic" 'face 'italic))
    (insert (propertize "Italic" 'face 'variable-pitch))
    (pop-to-buffer buffer)))

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
  (propertize text 'face 'slime-helpful-heading))

(defun slime-helpful-symbol (symbol-name)
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((symbol-info (slime-eval `(swank::read-elisp-symbol-info (swank::read-from-string ,symbol-name)))))
    (case (cdr (assoc :type symbol-info))
      (:function (slime-helpful-function symbol-name))
      (:package (slime-helpful-package symbol-name))
      (t (error "TODO")))))

;;(slime-helpful-symbol "ALEXANDRIA:FLATTEN")

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun slime-helpful-package (package-name)
  (interactive (list (slime-read-package-name "Describe package: ")))
  (when (not package-name)
    (error "No package name given"))

  (let* ((package-info (slime-eval `(swank::read-elisp-package-info ,package-name)))
         (buffer (get-buffer-create (format "*slime-helpful: %s package*" package-name))))
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
	(insert (propertize (prin1-to-string (cdr (assoc :type symbol-info))) 'face 'slime-helpful-type))
          (insert " ")
	  (insert-button (princ (cdr (assoc :name symbol-info)))
			 'action (lambda (btn)
				   (slime-helpful-symbol (prin1-to-string (cdr (assoc :symbol symbol-info))))))
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
      (pop-to-buffer buffer))))

;;(slime-helpful-package "ALEXANDRIA")

(defun slime-helpful-function (symbol-name)
  (interactive (list (slime-read-symbol-name "Describe symbol's function: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let* ((symbol-info (slime-eval `(swank::read-elisp-symbol-info (swank::read-from-string ,symbol-name))))
         (package-name (cdr (assoc :package symbol-info)))
         (buffer (get-buffer-create (format "*slime-helpful: %s function*" symbol-name))))
    (with-current-buffer buffer
      (insert (sh--propertize-heading (cdr (assoc :name symbol-info))))
      (newline 2)
      (insert (format "This is a FUNCTION in package "))
      (insert-button package-name
                     'action (lambda (btn)
                               (slime-helpful-package package-name)))
      (newline 2)
      (insert (sh--propertize-heading "Signature"))
      (newline)
      (insert (propertize (cdr (assoc :args symbol-info)) 'face lisp-cl-font-lock-keywords))
      (newline 2)
      (render-parsed-docstring (cdr (assoc :parsed-documentation symbol-info)))
      (newline 2)
      (cl-flet ((goto-source (btn)
                             (slime-edit-definition-other-window (cdr (assoc :symbol symbol-info)))))
        (insert-button "Source"
                       'action (function goto-source)))
      (insert " ")
      (cl-flet ((browse-references (btn)
                                   (slime-who-calls (cdr (assoc :name symbol-info)))))
        (insert-button "References"
                       'action (function browse-references)
                       'help-echo "Click button"))
      (insert " ")
      (insert-button "Disassemble") (insert " ")
      (cl-flet ((lookup-in-info (btn)
                                (info-apropos (cdr (assoc :name symbol-info)))))
        (insert-button "Lookup in manual"
                       'action (function lookup-in-info)))
      (setq buffer-read-only t)
      (local-set-key "q" 'kill-current-buffer)
      (buffer-disable-undo)
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (pop-to-buffer buffer))))

;;(slime-helpful-function "ALEXANDRIA:FLATTEN")
