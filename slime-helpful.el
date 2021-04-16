(let ((buffer (get-buffer-create "*slime-helpful*")))
  (with-current-buffer buffer
    (insert "hello")
    (insert "\n")
    (insert (propertize "Function" 'face 'bold))
    (insert "\n")
    (insert (propertize "Italic" 'face 'italic))
    (insert (propertize "Italic" 'face 'variable-pitch))
    (display-buffer buffer)))

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

(let ((symbol-info (slime-eval `(swank::read-elisp-symbol-info 'split-sequence:split-sequence))))
  (let ((buffer (get-buffer-create "*slime-helpful*")))
    (with-current-buffer buffer
      (render-parsed-docstring (cdr (assoc :parsed-documentation symbol-info)))
      (display-buffer buffer))))
