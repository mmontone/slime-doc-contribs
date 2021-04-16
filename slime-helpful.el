(setq lexical-binding t)

(require 'cl)

(let ((buffer (get-buffer-create "*slime-helpful*")))
  (with-current-buffer buffer
    (insert "hello")
    (insert "\n")
    (insert (propertize "Function" 'face 'bold))
    (insert "\n")
    (insert (propertize "Italic" 'face 'italic))
    (insert (propertize "Italic" 'face 'variable-pitch))
    (pop-to-buffer buffer)))

(slime-eval `(swank::read-elisp-symbol-info 'alexandria:flatten))

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
  (propertize text 'face 'bold))

(let ((symbol-info (slime-eval `(swank::read-elisp-symbol-info 'split-sequence:split-sequence))))
  (let ((buffer (get-buffer-create "*slime-helpful*")))
    (with-current-buffer buffer
      (insert (sh--propertize-heading (cdr (assoc :name symbol-info))))
      (newline 2)
      (insert (format "This is a FUNCTION in package %s" (cdr (assoc :package symbol-info))))
      (newline 2)
      (insert (sh--propertize-heading "Signature"))
      (newline)
      (insert (propertize (cdr (assoc :args symbol-info)) 'face lisp-cl-font-lock-keywords))
      (newline 2)
      (render-parsed-docstring (cdr (assoc :parsed-documentation symbol-info)))
      (newline 2)
      (cl-flet ((goto-source (btn)
                          (slime-edit-definition-other-window (cdr (assoc :name symbol-info)))))
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
      (pop-to-buffer buffer))))
