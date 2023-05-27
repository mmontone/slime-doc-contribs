;;; slime-help.el --- Fancier SLIME documentation buffers      -*- lexical-binding: t -*-

;; Copyright (C) 2021 Mariano Montone

;; Author: Mariano Montone <marianomontone@gmail.com>
;; URL: https://github.com/mmontone/slime-doc-contribs
;; Keywords: help, lisp, slime, common-lisp
;; Version: 0.1
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; slime-help is a SLIME extension that improves SLIME documentation buffers.

;;; Code:

(require 'map)
(require 'button)
(require 'lisp-mode)
(require 'slime)
(require 'subr-x)
(require 'cl-lib)

(defgroup slime-help nil
  "Common Lisp documentation browser."
  :prefix "slime-help-"
  :group 'slime)

(defcustom slime-help-ansicl-lookup-function
  'slime-hyperspec-lookup
  "Function for looking up a symbol in ANSICL spec."
  :type 'symbol
  :group 'slime-help)

(defun slime-help-ansicl-lookup (symbol-name)
  "Lookup SYMBOL-NAME in ANSICL spec."
  (interactive (list (common-lisp-hyperspec-read-symbol-name
                      (slime-symbol-at-point))))
  (funcall slime-help-ansicl-lookup-function symbol-name))

(defgroup slime-help-faces nil
  "Faces of SLIME-HELP."
  :group 'slime-help)

(defface slime-help-heading-1
  '((t :weight bold
       :height 1.4
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
       :height 1.2
       ;;:underline t
       ))
  "Slime help face for headings"
  :group 'slime-help-faces)

(defface slime-help-argument
  '((t (:inherit font-lock-type-face)))
  "Face for variables in Slime help"
  :group 'slime-help-faces)

(defface slime-help-variable
  '((t :foreground "darkgreen"))
  "Face for variables in Slime help"
  :group 'slime-help-faces)

(defface slime-help-keyword
  '((t :foreground "violet red"))
  "Face for keywords in Slime help"
  :group 'slime-help-faces)

(defface slime-help-name
  '((t :foreground "orange"))
  "Face for name in Slime help"
  :group 'slime-help-faces)

(defface slime-help-type
  '((t :foreground "purple"))
  "Face for type in Slime help"
  :group 'slime-help-faces)

(defface slime-help-apropos-label
  '((t :weight bold))
  "Slime help face for apropos items"
  :group 'slime-help-faces)

(defface slime-help-info
  '((t :foreground "green"))
  "Face for displaying slime-help information"
  :group 'slime-help-faces)

(defface slime-help-warning
  '((t :foreground "orange"))
  "Face for displaying slime-help warnings"
  :group 'slime-help-faces)

(defface slime-help-error
  '((t :foreground "red"))
  "Face for displaying slime-help errors"
  :group 'slime-help-faces)

(defface slime-help-button
  '((t (:box (:line-width 2 :color "dark grey") :background "light grey" :foreground "black")))
  "Face for slime-help buttons"
  :group 'slime-help-faces)

(defun slime-help--heading-1 (text)
  (propertize text 'face 'slime-help-heading-1))

(defun slime-help--heading-2 (text)
  (propertize text 'face 'slime-help-heading-2))

(defun slime-help--heading-3 (text)
  (propertize text 'face 'slime-help-heading-3))

(defun slime-help--horizontal-line (&rest width)
  (make-string (or width 80) ?\u2500))

(defun slime-help--info (text)
  (propertize text 'face 'slime-help-info))

(defun slime-help--warning (text)
  (propertize text 'face 'slime-help-warning))

(defun slime-help--error (text)
  (propertize text 'face 'slime-help-error))

(defun slime-help--propertize-docstring (string)
  (slime-help--propertize-links
   (slime-help--propertize-bare-links string)))

(defun slime-help--insert-documentation (info &optional package)
  (if slime-help-parse-docstrings
      (slime-help--format-parsed-docstring
       (cdr (assoc :parsed-documentation info))
       (or package (cdr (assoc :package info))))
    (insert (slime-help--propertize-docstring (cdr (assoc :documentation info))))))

;; copied from helpful.el library

(defun slime-help--button (text type &rest properties)
  ;; `make-text-button' mutates our string to add properties. Copy
  ;; TEXT to prevent mutating our arguments, and to support 'pure'
  ;; strings, which are read-only.
  (setq text (substring-no-properties text))
  (apply #'make-text-button
         text nil
         :type type
         properties))

(define-button-type 'slime-help-link-button
  'action #'slime-help--follow-link
  'follow-link t
  'help-echo "Follow this link")

(define-button-type 'slime-help-lookup-in-manuals-button
  'action #'slime-help--lookup-in-manuals
  'follow-link t
  'face 'slime-help-button
  'help-echo "Lookup in manuals")

(defun slime-help--lookup-in-manuals (btn)
  (funcall slime-help-lookup-in-manuals-function
           (prin1-to-string (button-get btn 'symbol))))

(defun slime-help--propertize-links (docstring)
  "Convert URL links in docstrings to buttons."
  (replace-regexp-in-string
   (rx "URL `" (group (*? any)) "'")
   (lambda (match)
     (let ((url (match-string 1 match)))
       (concat "URL "
               (slime-help--button
                url
                'slime-help-link-button
                'url url))))
   docstring))

(defun slime-help--propertize-bare-links (docstring)
  "Convert URL links in docstrings to buttons."
  (replace-regexp-in-string
   (rx (group (or string-start space "<"))
       (group "http" (? "s") "://" (+? (not (any space))))
       (group (? (any "." ">" ")"))
              (or space string-end ">")))
   (lambda (match)
     (let ((space-before (match-string 1 match))
           (url (match-string 2 match))
           (after (match-string 3 match)))
       (concat
        space-before
        (slime-help--button
         url
         'slime-help-link-button
         'url url)
        after)))
   docstring))

(defun slime-help--follow-link (button)
  "Follow the URL specified by BUTTON."
  (browse-url (button-get button 'url)))

;; helpful.el stuff ends here

(defun slime-help--qualify-cl-symbol-name (symbol-name &optional package)
  "Qualify a Common Lisp SYMBOL-NAME using PACKAGE.
If SYMBOL-NAME is already qualified, nothing is done.
If PACKAGE is not given, SLIME-CURRENT-PACKAGE is used instead."
  (let ((package (or package (slime-current-package))))
    (if (cl-position ?: symbol-name)
        ;; already qualified
        symbol-name
      (format "%s::%s" package symbol-name))))

(defun slime-help--format-parsed-docstring (docstring _package)
  (dolist (word docstring)
    (cond
     ((stringp word) (insert (slime-help--propertize-docstring word)))
     ((and (listp word) (eql (cl-first word) :arg))
      (insert (propertize (cl-second word) 'face 'slime-help-argument)))
     ((and (listp word) (eql (cl-first word) :fn))
      (insert-button (cl-second word)
                     'action (lambda (_btn)
                               (slime-help-function (third word)))
                     'follow-link t
                     'help-echo "Describe function"))
     ((and (listp word) (eql (cl-first word) :macro))
      (insert-button (cl-second word)
                     'action (lambda (_btn)
                               (slime-help-macro (third word)))
                     'follow-link t
                     'help-echo "Describe function"))
     ((and (listp word) (eql (cl-first word) :class))
      (insert-button (cl-second word)
                     'action (lambda (_btn)
                               (slime-help-class (third word)))
                     'follow-link t
                     'help-echo "Describe class"))
     ((and (listp word) (eql (cl-first word) :key))
      (insert (propertize (cl-second word) 'face 'slime-help-keyword)))
     ((and (listp word) (eql (cl-first word) :var))
      (insert-button (cl-second word)
                     'action (lambda (_btn)
                               (slime-help-variable (third word)))
                     'follow-link t
                     'help-echo "Describe variable"))
     ((and (listp word) (eql (cl-first word) :special-operator))
      (insert-button (cl-second word)
                     'action (lambda (_btn)
                               (slime-help-special-operator (third word)))
                     'follow-link t
                     'help-echo "Describe special operator"))
     (t (error "Don't know how to format: %s" word)))))

(defun slime-help-symbol (symbol-name)
  "Open a help buffer for each kind of SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Describe symbol: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((symbol-infos (slime-eval `(swank-help:read-emacs-symbol-info (cl:read-from-string ,(slime-qualify-cl-symbol-name symbol-name))))))
    (dolist (symbol-info symbol-infos)
      (cl-case (cdr (assoc :type symbol-info))
        (:function (slime-help-function symbol-name))
        (:generic-function (slime-help-generic-function symbol-name))
        (:macro (slime-help-macro symbol-name))
        (:package (slime-help-package symbol-name))
        (:variable (slime-help-variable symbol-name))
        (:class (slime-help-class symbol-name))
        (t (error "TODO"))))))

;;(slime-help-symbol "ALEXANDRIA:FLATTEN")
;;(slime-help-symbol "ALEXANDRIA:WITH-GENSYMS")

(defun slime-help--first-line (string)
  "Return the first line of the `STRING'."
  (let ((pos (cl-position ?\n string)))
    (if (null pos) string (cl-subseq string 0 pos))))

(defun slime-help--kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun slime-help-quit ()
  "Kill all slime-help buffers at once."
  (interactive)
  (mapcar 'kill-buffer
          (cl-remove-if-not
           (lambda (buffer)
             (string-prefix-p "*slime-help" (buffer-name buffer)))
           (buffer-list))))

(defun slime-help--open-buffer ()
  (let ((buffer (current-buffer)))
    (setq buffer-read-only t)
    (local-set-key "q" 'slime-help--kill-current-buffer)
    (buffer-disable-undo)
    (set (make-local-variable 'kill-buffer-query-functions) nil)
    (slime-mode)
    (slime-help-mode)
    (goto-char 0)
    (pop-to-buffer buffer)))

(cl-defun slime-help-package (package-name)
  "Display information about Common Lisp package named PACKAGE-NAME."
  (interactive (list (slime-read-package-name "Describe package: ")))
  (when (not package-name)
    (error "No package name given"))

  (let ((buffer-name (format "*slime-help: %s package*" package-name)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from slime-help-package))
    (let* ((package-info (slime-eval `(swank-help:read-emacs-package-info ,package-name)))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (insert (slime-help--heading-1 (upcase (string-trim package-name))))
        (newline 2)
        (insert (format "This is a Common Lisp package with %d external symbols" (length (cdr (assoc :external-symbols package-info)))))
        (newline 2)
        (when (cdr (assoc :documentation package-info))
          (insert (slime-help--propertize-docstring (cdr (assoc :documentation package-info))))
          (newline 2))

        (cl-flet ((goto-source (_btn)
                               (slime-edit-definition-other-window package-name)))
          (insert-button "Source"
                         'action (function goto-source)
                         'face 'slime-help-button
                         'follow-link t
                         'help-echo "Go to package source code"))
        (newline 2)

        (insert (slime-help--heading-2 "Exported definitions"))
        (newline 2)
        (cl-flet ((format-exported-definition
                   (symbol-info)
                   (insert (propertize (cl-subseq (symbol-name (cdr (assoc :type symbol-info))) 1) 'face 'slime-help-type))
                   (insert " ")
                   (insert-button (format "%s" (cdr (assoc :name symbol-info)))
                                  'action (lambda (_btn)
                                            (slime-help-symbol (prin1-to-string (cdr (assoc :symbol symbol-info)))))
                                  'follow-link t
                                  'help-echo "Describe symbol")
                   (newline)
                   (if (cdr (assoc :documentation symbol-info))
                       (insert (slime-help--first-line (cdr (assoc :documentation symbol-info))))
                     (insert "Not documented"))
                   (newline)
                   (insert (slime-help--horizontal-line))
                   (newline)))
          (let ((def-types '(("Variables" . :variable)
                             ("Classes" . :class)
                             ("Macros" . :macro)
                             ("Functions" . :function)
                             ("Generic functions" . :generic-function))))
            (dolist (def-type def-types)
              (let ((symbol-infos (cl-remove-if-not (lambda (x)
                                                      (cl-equalp (cdr (assoc :type x)) (cdr def-type)))
                                                    (cdr (assoc :external-symbols package-info)))))
                (when symbol-infos
                  (insert (slime-help--heading-3 (car def-type)))
                  (newline 2)
                  (dolist (symbol-info symbol-infos)
                    (format-exported-definition symbol-info))
                  (newline 2))))))
        (slime-help--open-buffer)
        nil))))

;;(slime-help-package "ALEXANDRIA")

(cl-defun slime-help-packages ()
  "Display information about Common Lisp packages."
  (interactive)

  (let ((buffer-name "*slime-help: COMMON LISP packages*"))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from slime-help-packages))

    (let* ((packages-info (slime-eval `(swank-help:read-emacs-packages-info)))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (dolist (package-info packages-info)
          (let ((package-name (cdr (assoc :name package-info))))
            (insert-button package-name
                           'action (lambda (btn)
                                     (ignore btn)
                                     (slime-help-package package-name))
                           'follow-link t
                           'help-echo "Describe package"))
          (newline)
          (when (cdr (assoc :documentation package-info))
            (insert (cdr (assoc :documentation package-info)))
            (newline)))
        (slime-help--open-buffer)
        nil))))

(cl-defun slime-help-systems ()
  "Display information about registered ASDF systems."

  (interactive)

  (let ((buffer-name "*slime-help: registered ASDF systems*"))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from slime-help-systems))

    (let* ((systems-info (slime-eval `(swank-help:read-emacs-systems-info)))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (dolist (system-info systems-info)
          (lexical-let ((cl-system-name (cdr (assoc :name system-info))))
            (insert-button cl-system-name
                           'action (lambda (btn)
                                     (ignore btn)
                                     (slime-help-system cl-system-name))
                           'follow-link t
                           'help-echo "Describe system"))
          (newline)
          (when (cdr (assoc :documentation system-info))
            (insert (cdr (assoc :documentation system-info)))
            (newline)))
        (slime-help--open-buffer)
        nil))))

(defun slime-help-function (symbol-name)
  "Display documentation about Common Lisp function bound to SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Describe function: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((symbol-info (slime-eval `(swank-help:read-emacs-symbol-info (cl:read-from-string ,(slime-qualify-cl-symbol-name symbol-name)) :function))))
    (when (null symbol-info)
      (error "Could not read symbol information %s" symbol-name))
    (slime-help--funcallable symbol-name symbol-info :function)))

;; (slime-help-function "ALEXANDRIA:FLATTEN")

(defun slime-help-macro (symbol-name)
  "Display documentation about Common Lisp macro bound to SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Describe macro: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((symbol-info (slime-eval `(swank-help:read-emacs-symbol-info (cl:read-from-string ,(slime-qualify-cl-symbol-name symbol-name)) :macro))))
    (when (null symbol-info)
      (error "Could not read symbol information: %s" symbol-name))
    (slime-help--funcallable symbol-name symbol-info
                             :macro
                             'extra-buttons
                             (lambda ()
                               (cl-flet ((browse-expanders (btn)
                                                           (ignore btn)
                                                           (slime-who-macroexpands (prin1-to-string (cdr (assoc :symbol symbol-info))))))
                                 (insert-button "Expanders"
                                                'face 'slime-help-button
                                                'action (function browse-expanders)
                                                'follow-link t
                                                'help-echo "Show all known expanders of the macro"))
                               (insert " ")))))

;; (slime-help-macro "ALEXANDRIA:WITH-GENSYMS")

(cl-defun slime-help-special-operator (symbol-name)
  "Display documentation about Common Lisp macro bound to SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Describe special operator: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((symbol-info (slime-eval `(swank-help:read-emacs-symbol-info (cl:read-from-string ,(slime-qualify-cl-symbol-name symbol-name)) :special-operator))))
    (when (null symbol-info)
      (error "Could not read symbol information: %s" symbol-name))

    (let ((buffer-name (format "*slime-help: %s special operator*" symbol-name)))

      (when (get-buffer buffer-name)
        (pop-to-buffer buffer-name)
        (cl-return-from slime-help-special-operator))

      (let* ((package-name (cdr (assoc :package symbol-info)))
             (buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (insert (slime-help--heading-1 (cdr (assoc :name symbol-info))))
          (newline 2)
          (insert (format "This is a special operator in package "))
          (insert-button package-name
                         'action (lambda (btn)
                                   (ignore btn)
                                   (slime-help-package package-name))
                         'follow-link t
                         'help-echo "Describe package")

          (when (cdr (assoc :documentation symbol-info))
            (newline 2)
            (slime-help--insert-documentation symbol-info)
            (newline 2))

          (when (cl-member (cdr (assoc :package symbol-info))
                           '("COMMON-LISP" "CL") :test 'cl-equalp)
            (insert-button "Lookup in ANSICL spec"
                           'face 'slime-help-button
                           'action (lambda (_btn)
                                     (funcall slime-help-ansicl-lookup-function
                                              (prin1-to-string (cdr (assoc :symbol symbol-info)))))
                           'follow-link t
                           'help-echo "Lookup variable in ANSICL spec"))

          (slime-help--open-buffer)
          nil)))))

;; (slime-help-special-operator "CL:IF")

(defun slime-help-generic-function (symbol-name)
  "Display documentation about Common Lisp generic function bound to SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Describe generic function: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (let ((symbol-info (slime-eval `(swank-help:read-emacs-symbol-info (cl:read-from-string ,(slime-qualify-cl-symbol-name symbol-name)) :generic-function))))
    (when (null symbol-info)
      (error "Could not read symbol information %s" symbol-name))
    (slime-help--funcallable
     symbol-name symbol-info
     :generic-function
     'continuation
     (lambda ()
       (let ((methods (slime-find-definitions (slime-qualify-cl-symbol-name symbol-name))))
         (newline 2)
         (insert (slime-help--heading-2 "Methods"))
         (newline 2)
         (cl-loop for (label location) in methods do
                  (slime-insert-propertized
                   (list 'slime-location location
                         'face 'font-lock-keyword-face)
                   "  " (slime-one-line-ify label) "\n")))
       (slime-xref-mode)))))

;; (slime-help-generic-function "CL:PRINT-OBJECT")

;; TODO: remove the following function. I think it is better to implement individual functions for
;; each type of funcallable (macro, function, generic-functions), as they have significant differences.
(cl-defun slime-help--funcallable (symbol-name symbol-info function-type &rest args)
  "Display documentation about CL funcallable FUNCTION-TYPE to SYMBOL-NAME.
SYMBOL-INFO contains the collected SWANK documentation.
ARGS contains additional arguments, like 'extra-buttons."
  (let* ((function-type-name (cl-subseq (symbol-name function-type) 1))
         (buffer-name (format "*slime-help: %s %s*" symbol-name function-type-name)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from slime-help--funcallable))

    (let* ((package-name (cdr (assoc :package symbol-info)))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (insert (slime-help--heading-1 (cdr (assoc :name symbol-info))))
        (newline 2)
        (insert (format "This is a %s in package " function-type-name))
        (insert-button package-name
                       'action (lambda (btn)
                                 (ignore btn)
                                 (slime-help-package package-name))
                       'follow-link t
                       'help-echo "Describe package")
        (newline 2)
        (insert (slime-help--heading-3 "Signature"))
        (newline)
        (insert (slime-help--highlight-syntax (cdr (assoc :args symbol-info))))
        (newline 2)

        (when (cdr (assoc :documentation symbol-info))
          (slime-help--insert-documentation symbol-info)
          (newline 2))

        (cl-flet ((goto-source (btn)
                               (ignore btn)
                               (slime-edit-definition-other-window (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "Source"
                         'action (function goto-source)
                         'face 'slime-help-button
                         'follow-link t
                         'help-echo "Go to definition source code"))
        (insert " ")

        (cl-flet ((browse-references (_btn)
                                     (slime-who-calls (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "References"
                         'action (function browse-references)
                         'face 'slime-help-button
                         'follow-link t
                         'help-echo "Browse references"))
        (insert " ")

        (cl-flet ((disassemble-function (_btn)
                                        (slime-disassemble-symbol (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "Disassemble"
                         'action (function disassemble-function)
                         'face 'slime-help-button
                         'follow-link t
                         'help-echo "Disassemble function"))
        (insert " ")

        (when (cl-getf args 'extra-buttons)
          (funcall (cl-getf args 'extra-buttons)))

        (insert (slime-help--button "Lookup in manuals"
                                    'slime-help-lookup-in-manuals-button
                                    'symbol (cdr (assoc :symbol symbol-info))))
        (insert " ")

        (when (cl-member (cdr (assoc :package symbol-info))
                         '("COMMON-LISP" "CL") :test 'cl-equalp)
          (insert-button "Lookup in ANSICL spec"
                         'face 'slime-help-button
                         'action (lambda (_btn)
                                   (funcall slime-help-ansicl-lookup-function
                                            (prin1-to-string (cdr (assoc :symbol symbol-info)))))
                         'follow-link t
                         'help-echo "Lookup variable in ANSICL spec"))

        ;; TODO: add a collapsible extra section with debugging actions, like toggle tracing, toggle profiling, perhaps disassemble too.

        (when (cl-getf args 'continuation)
          (funcall (cl-getf args 'continuation)))

        (slime-help--open-buffer)
        nil))))

;;(slime-help-function "CL:REMOVE")
;;(slime-help-function "ALEXANDRIA:FLATTEN")
;;(slime-help-function "SPLIT-SEQUENCE:SPLIT-SEQUENCE")
;;(slime-help-macro "ALEXANDRIA:WITH-GENSYMS")

(cl-defun slime-help-variable (symbol-name)
  "Display documentation about Common Lisp variable bound to SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Describe variable: ")))
  (when (not symbol-name)
    (error "No symbol given"))

  (let ((buffer-name (format "*slime-help: %s variable*" symbol-name)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from slime-help-variable))

    (let* ((symbol-info (slime-eval `(swank-help:read-emacs-symbol-info (cl:read-from-string ,(slime-qualify-cl-symbol-name symbol-name)) :variable)))
           (package-name (cdr (assoc :package symbol-info)))
           (buffer (get-buffer-create buffer-name)))
      (when (null symbol-info)
        (error "Could not read variable info"))
      (with-current-buffer buffer
        (insert (slime-help--heading-1 (cdr (assoc :name symbol-info))))
        (newline 2)
        (insert (format "This is a VARIABLE in package "))
        (insert-button package-name
                       'action (lambda (btn)
                                 (ignore btn)
                                 (slime-help-package package-name))
                       'follow-link t
                       'help-echo "Describe package")
        (newline 2)
        (when (cdr (assoc :documentation symbol-info))
          (slime-help--insert-documentation symbol-info)
          (newline 2))

        (if (not (cdr (assoc :boundp symbol-info)))
            (insert (slime-help--warning "The variable is UNBOUND"))
          (progn
            (insert (propertize "Value: " 'face 'bold))
            (insert (slime-help--info (cdr (assoc :value symbol-info))))))
        (newline 2)

        (cl-flet ((goto-source (btn)
                               (ignore btn)
                               (slime-edit-definition-other-window (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "Source"
                         'action (function goto-source)
                         'face 'slime-help-button
                         'follow-link t
                         'help-echo "Go to definition source code"))
        (insert " ")

        (cl-flet ((browse-references (_btn)
                                     (slime-who-references (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "References"
                         'face 'slime-help-button
                         'action (function browse-references)
                         'follow-link t
                         'help-echo "Browse references"))
        (insert " ")

        (cl-flet ((browse-binders (_btn)
                                  (slime-who-binds (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "Binders"
                         'face 'slime-help-button
                         'action (function browse-binders)
                         'follow-link t
                         'help-echo "Show all known binders of the global variable"))
        (insert " ")

        (cl-flet ((browse-setters (_btn)
                                  (slime-who-sets (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "Setters"
                         'face 'slime-help-button
                         'action (function browse-setters)
                         'follow-link t
                         'help-echo "Show all known setters of the global variable"))
        (insert " ")

        (insert (slime-help--button "Lookup in manuals"
                                    'slime-help-lookup-in-manuals-button
                                    'symbol (cdr (assoc :symbol symbol-info))))

        (insert " ")

        (when (cl-member (cdr (assoc :package symbol-info))
                         '("COMMON-LISP" "CL") :test 'cl-equalp)
          (insert-button "Lookup in ANSICL spec"
                         'face 'slime-help-button
                         'action (lambda (_btn)
                                   (funcall slime-help-ansicl-lookup-function
                                            (prin1-to-string (cdr (assoc :symbol symbol-info)))))
                         'follow-link t
                         'help-echo "Lookup variable in ANSICL spec"))

        (slime-help--open-buffer)
        nil))))

;; (slime-help-variable "*STANDARD-OUTPUT*")

(cl-defun slime-help-class (symbol-name)
  "Display documentation about Common Lisp class bound to SYMBOL-NAME."
  (interactive (list (slime-read-symbol-name "Describe class: ")))
  (when (not symbol-name)
    (error "No symbol given"))

  (let ((buffer-name (format "*slime-help: %s class*" symbol-name)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from slime-help-class))

    (let* ((symbol-info (slime-eval `(swank-help:read-emacs-symbol-info (cl:read-from-string ,(slime-qualify-cl-symbol-name symbol-name)) :class)))
           (package-name (cdr (assoc :package symbol-info)))
           (buffer (get-buffer-create buffer-name)))
      (when (null symbol-info)
        (error "Could not read class info"))
      (with-current-buffer buffer
        (insert (slime-help--heading-1 (cdr (assoc :name symbol-info))))
        (newline 2)
        (insert (format "This is a CLASS in package "))
        (insert-button package-name
                       'action (lambda (_btn)
                                 (slime-help-package package-name))
                       'follow-link t
                       'help-echo "Describe package")
        (newline 2)

        (when (cdr (assoc :documentation symbol-info))
          (slime-help--insert-documentation symbol-info)
          (newline 2))

        ;; buttons
        (cl-flet ((goto-source (_btn)
                               (slime-edit-definition-other-window (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "Source"
                         'face 'slime-help-button
                         'action (function goto-source)
                         'follow-link t
                         'help-echo "Go to definition source code"))
        (insert " ")
        (cl-flet ((browse-references (_btn)
                                     (slime-who-references (prin1-to-string (cdr (assoc :symbol symbol-info))))))
          (insert-button "References"
                         'face 'slime-help-button
                         'action (function browse-references)
                         'follow-link t
                         'help-echo "Browse references"))
        (insert " ")

        (insert (slime-help--button "Lookup in manuals"
                                    'slime-help-lookup-in-manuals-button
                                    'symbol (cdr (assoc :symbol symbol-info))))
        (insert " ")

        (when (cl-member (cdr (assoc :package symbol-info))
                         '("COMMON-LISP" "CL") :test 'cl-equalp)
          (insert-button "Lookup in ANSICL spec"
                         'face 'slime-help-button
                         'action (lambda (_btn)
                                   (funcall slime-help-ansicl-lookup-function
                                            (prin1-to-string (cdr (assoc :symbol symbol-info)))))
                         'follow-link t
                         'help-echo "Lookup variable in ANSICL spec"))

        (newline 2)

        (insert (slime-help--heading-2 "Direct superclasses"))
        (newline 2)
        (dolist (class-name (cdr (assoc :direct-superclasses symbol-info)))
          (insert-button (upcase (symbol-name class-name))
                         'action (lambda (_btn)
                                   (slime-help-class (symbol-name class-name)))
                         'follow-link t
                         'help-echo "Describe class")
          (insert " "))
        (newline 2)

        ;; TODO: show a collapsable (outline-mode?) section with more information about the class
        ;; like class descendants and list of subclasses
        ;; let's show direct-subclasses for now, with a limit
        (let ((max-subclasses 25)
              (subclasses (cdr (assoc :direct-subclasses symbol-info))))
          (when (not (zerop (length subclasses)))
            (insert (slime-help--heading-2 "Direct subclasses"))
            (newline 2)
            (dolist (class-name (cl-subseq subclasses 0 (min max-subclasses (length subclasses))))
              (insert-button (upcase (symbol-name class-name))
                             'action (lambda (_btn)
                                       (slime-help-class (symbol-name class-name)))
                             'follow-link t
                             'help-echo "Describe class")
              (insert " "))
            (when (> (length subclasses) max-subclasses)
              (insert "and more"))
            (newline 2)))

        ;; TODO: show more information about slots
        (let ((slots (cdr (assoc :slots symbol-info))))
          (insert (slime-help--heading-2 "Slots"))
          (newline 2)
          (if (zerop (length slots))
              (progn (insert "No slots") (newline))
            (dolist (slot slots)
              (insert (propertize (format "- %s" (cdr (assoc :name slot))) 'face 'bold))
              (newline)
              (when (cdr (assoc :documentation slot))
                (slime-help--insert-documentation slot (cdr (assoc :package symbol-info)))
                (newline))))
          (newline))

        (let ((methods-start (point))
              (methods (cdr (assoc :methods symbol-info))))
          (insert (slime-help--heading-2 "Methods"))
          (make-text-button methods-start (point)
                            'action (lambda (_btn)
                                      (goto-char methods-start)
                                      (outline-toggle-children))
                            'follow-link t)
          (newline 2)
          (if (zerop (length methods))
              (insert "No methods")
            (dolist (symbol-info methods)
              (insert-button (format "%s" (upcase (prin1-to-string (cdr (assoc :name symbol-info)))))
                             'action (lambda (_btn)
                                       (slime-help-symbol (prin1-to-string (cdr (assoc :name symbol-info)))))
                             'follow-link t
                             'help-echo "Describe symbol")
              (newline)
              (if (cdr (assoc :documentation symbol-info))
                  (insert (slime-help--first-line (cdr (assoc :documentation symbol-info))))
                (insert "Not documented"))
              (newline)
              (insert (slime-help--horizontal-line))
              (newline))))

        ;; Outlines configuration
        ;;(setq outline-regexp "Methods")
        ;;(outline-minor-mode)
        ;;(outline-hide-body)

        (slime-help--open-buffer)

        nil))))

;;(slime-help-class "HUNCHENTOOT:ACCEPTOR")

;; This was copied from help.el
(defun slime-help--highlight-syntax (source &optional mode)
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
              (setq hook-funcs (append hook-funcs funcs)))))

        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))
        (buffer-string))
    ;; SOURCE was too long to highlight in a reasonable amount of
    ;; time.
    source))

(cl-defun slime-help-system (cl-system-name)
  "Display documentation about ASDF system named CL-SYSTEM-NAME."
  (interactive (list (slime-read-system-name "Describe system")))
  (when (not cl-system-name)
    (error "No system name given"))

  (let ((buffer-name (format "*slime-help: %s system*" cl-system-name)))
    (when (get-buffer buffer-name)
      (pop-to-buffer buffer-name)
      (cl-return-from slime-help-system))

    (let* ((system-info (slime-eval `(swank-help:read-emacs-system-info ,cl-system-name)))
           (buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (insert (slime-help--heading-1 (upcase cl-system-name)))
        (newline 2)
        (insert (format "This is a Common Lisp ASDF system with %d dependencies" (length (cdr (assoc :dependencies system-info)))))
        (newline)
        (if (cdr (assoc :loaded-p system-info))
            (insert (slime-help--info "This system is already loaded."))
          (insert (slime-help--error "This system is not loaded.")))
        (newline 2)
        (when (cdr (assoc :documentation system-info))
          (insert (slime-help--propertize-docstring (cdr (assoc :documentation system-info))))
          (newline 2))

        ;; buttons

        (cl-flet ((browse-system (_btn)
                                 (slime-browse-system cl-system-name)))
          (insert-button "Browse"
                         'action (function browse-system)
                         'follow-link t
                         'help-echo "Browse system"))

        (insert " ")

        (when (not (cdr (assoc :loaded-p system-info)))
          (cl-flet ((load-system (_btn)
                                 (slime-load-system cl-system-name)))
            (insert-button "Load"
                           'action (function load-system)
                           'follow-link t
                           'help-echo "Load system")))

        (newline 2)

        (insert (slime-help--heading-2 "Dependencies"))
        (newline 2)
        (if (zerop (length (cdr (assoc :dependencies system-info))))
            (insert "It has no dependencies")
          ;; else
          (dolist (dependency (cdr (assoc :dependencies system-info)))
            (insert "* ")
            (insert-button dependency
                           'action (lambda (_btn)
                                     (slime-help-system dependency))
                           'follow-link t
                           'help-echo "Describe system")
            (newline)))

        (when (and (cdr (assoc :loaded-p system-info))
                   (cdr (assoc :packages system-info)))
          (newline 2)
          (insert (slime-help--heading-2 "Packages"))
          (newline 2)
          (dolist (package-name (cdr (assoc :packages system-info)))
            (insert-button package-name
                           'action (lambda (_btn)
                                     (slime-help-package package-name))
                           'follow-link t
                           'help-echo "Describe package")
            (insert " ")))

        (slime-help--open-buffer)
        nil))))

;;(slime-help-system "alexandria")

(defun slime-help-print-apropos (plists)
  (dolist (plist plists)
    (let ((designator (plist-get plist :designator)))
      (cl-assert designator)
      ;;(slime-insert-propertized `(face slime-apropos-symbol) designator)
      (insert (propertize designator 'face 'slime-apropos-symbol)))
    (terpri)
    (cl-loop for (prop value) on plist by #'cddr
             unless (eq prop :designator) do
             (let ((namespace (cadr (or (assq prop slime-apropos-namespaces)
                                        (error "Unknown property: %S" prop))))
                   (start (point)))
               (princ "  ")
               (slime-insert-propertized `(face slime-help-apropos-label) namespace)
               (princ ": ")
               (princ (cl-etypecase value
                        (string value)
                        ((member nil :not-documented) "(not documented)")))
               (let ((designator (plist-get plist :designator)))
                 (add-text-properties
                  start (point)
                  (list 'type prop
                        'action (lambda (btn) (ignore btn) (slime-help-symbol designator))
                        'weight 'bold
                        'button t
                        'apropos-label namespace
                        'item designator)))
               (terpri)))))

(defun slime-help-show-apropos (plists string package summary)
  (if (null plists)
      (message "No apropos matches for %S" string)
    (slime-with-popup-buffer ((slime-buffer-name :apropos)
                              :package package :connection t
                              :mode 'apropos-mode)
      (if (boundp 'header-line-format)
          (setq header-line-format summary)
        (insert summary "\n\n"))
      (slime-set-truncate-lines)
      (slime-help-print-apropos plists)
      (set-syntax-table lisp-mode-syntax-table)
      (goto-char (point-min)))))

(defun slime-help-apropos (string &optional only-external-p package
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
    (slime-eval-async
        `(swank:apropos-list-for-emacs ,string ,only-external-p
                                       ,case-sensitive-p ',package)
      (slime-rcurry #'slime-help-show-apropos string buffer-package
                    (slime-apropos-summary string case-sensitive-p
                                           package only-external-p)))))

(defun slime-help-apropos-all ()
  "Shortcut for (slime-help-apropos <string> nil nil)"
  (interactive)
  (slime-help-apropos (read-string "SLIME Apropos: ") nil nil))

(defun slime-help-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (slime-read-package-name "Package: ")))
                       (if (string= pkg "") (slime-current-package) pkg))
                     current-prefix-arg))
  (slime-help-apropos "" (not internal) package))

(defun slime-help-apropos-documentation (pattern &optional package)
  "Show symbols whose documentation contains matches for PATTERN.
PATTERN can be a word, a list of words (separated by spaces),
or a regexp (using some regexp special characters).  If it is a word,
search for matches for that word as a substring.  If it is a list of words,
search for matches for any two (or more) of those words."
  (interactive (list (apropos-read-pattern "documentation")))

  (let ((buffer-package (or package (slime-current-package))))
    (slime-eval-async
        `(swank-help:apropos-documentation-for-emacs
          ',pattern t
          nil nil)
      (slime-rcurry #'slime-help-show-apropos (cl-first pattern) buffer-package
                    (slime-apropos-summary pattern nil
                                           nil t)))))

(defun slime-help ()
  (interactive)
  (slime-help-systems))

(defvar slime-help-mode-map
  (let ((map (make-keymap)))
    (define-key map "q" 'slime-help--kill-current-buffer)
    (define-key map "Q" 'slime-help-quit)
    map))

(define-minor-mode slime-help-mode
  "Quicklisp systems minor mode."
  :init-value nil
  :lighter " SLIME-Help"
  :keymap slime-help-mode-map
  :group 'slime-help)

(easy-menu-define
  slime-help-mode-menu slime-help-mode-map
  "Menu for SLIME-Help"
  '("SLIME Help"
    ["Browse systems" slime-help-systems
     :help "Browse registered ASDF systems"]
    ["Browse packages" slime-help-packages
     :help "Browse the list of loaded packages"]
    "---"
    ["Describe symbol..." slime-help-symbol
     :help "Show documentation of symbol"]
    ["Describe function..." slime-help-function
     :help "Show documentation of function"]
    ["Describe package..." slime-help-package
     :help "Show package documentation"]
    ["Describe system..." slime-help-system
     :help "Show ASDF system documentation"]
    "---"
    [ "Lookup Documentation..." slime-documentation-lookup t ]
    [ "Apropos..."              slime-help-apropos t]
    [ "Apropos all..."          slime-help-apropos-all t]
    [ "Apropos Package..."      slime-help-apropos-package t]
    [ "Apropos documentation..." slime-help-apropos-documentation
      :help "Search in docstrings"]
    "---"
    ["Quit" slime-help-quit
     :help "Quit SLIME help"]))

(easy-menu-define
  slime-help-submenu nil
  "Menu for SLIME-Help"
  '("Documentation"
    ["Browse systems" slime-help-systems
     :help "Browse registered ASDF systems"]
    ["Browse packages" slime-help-packages
     :help "Browse the list of loaded packages"]
    "---"
    ["Describe symbol..." slime-help-symbol
     :help "Show documentation of symbol"]
    ["Describe function..." slime-help-function
     :help "Show documentation of function"]
    ["Describe package..." slime-help-package
     :help "Show package documentation"]
    ["Describe system..." slime-help-system
     :help "Show ASDF system documentation"]
    "---"
    [ "Lookup Documentation..." slime-documentation-lookup t ]
    [ "Apropos..."              slime-help-apropos t]
    [ "Apropos all..."          slime-help-apropos-all t]
    [ "Apropos Package..."      slime-help-apropos-package t]
    [ "Apropos documentation..." slime-help-apropos-documentation
      :help "Search in docstrings"]
    [ "ANSI Common Lisp spec..."  slime-help-ansicl-lookup t ]
    "---"
    ["Quit" slime-help-quit
     :help "Quit SLIME help"]))

(defun slime-help-setup-key-bindings ()
  (define-key slime-doc-map "a" 'slime-help-apropos)
  (define-key slime-doc-map "z" 'slime-help-apropos-all)
  (define-key slime-doc-map "d" 'slime-help-symbol)
  (define-key slime-doc-map "f" 'slime-help-function)
  (define-key slime-doc-map "p" 'slime-help-package)
  (define-key slime-doc-map "s" 'slime-help-system))

(defun slime-help--add-menu-to-slime ()
  (easy-menu-add-item 'menubar-slime nil 'slime-help-mode-menu))

(defun slime-help--replace-slime-documentation-menu ()
  (easy-menu-add-item 'menubar-slime nil 'slime-help-submenu))

(define-slime-contrib slime-help
  "Augmented help mode for Common Lisp"
  (:authors "Mariano Montone")
  (:license "GPL")
  (:slime-dependencies slime-asdf)
  (:swank-dependencies swank-help)
  (:on-load
   ;; setup key bindings
   (slime-help-setup-key-bindings)
   ;; add submenu to SLIME menu
   ;;(slime-help--add-menu-to-slime)
   (slime-help--replace-slime-documentation-menu)))

(defcustom slime-help-parse-docstrings t
  "When enabled, docstrings are parsed and function arguments and code references are formatted accordingly."
  :type 'boolean
  :group 'slime-help)

(defcustom slime-help-lookup-in-manuals-function 'info-apropos
  "Function used to look up slime-help terms into manuals."
  :type 'symbol
  :group 'slime-help)

(provide 'slime-help)

;;; slime-help.el ends here
