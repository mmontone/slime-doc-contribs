;;; slime-info --- Slime info
;;; Commentary:
;;; This is a package

;;; Code:

(require 'slime)

(defun info-buffer-test ()
  (let ((temp-file (make-temp-file "info-buffer-test")))
    (with-temp-file temp-file
      (insert "@setfilename info-buffer-test.info
@settitle Sample Manual 1.0

@copying
This is a short example of a complete Texinfo file.

Copyright @copyright{} 2016 Free Software Foundation, Inc.
@end copying

@titlepage
@title Sample Title
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@c Output the table of the contents at the beginning.
@contents

@ifnottex
@node Top
@top GNU Sample

This manual is for GNU Sample
(version @value{VERSION}, @value{UPDATED}).
@end ifnottex

@menu
* First Chapter::    The first chapter is the
                      only chapter in this sample.
* Index::            Complete index.
@end menu

@node First Chapter
@chapter First Chapter

@cindex chapter, first

This is the first chapter.
@cindex index entry, another

Here is a numbered list.

@enumerate
@item
This is the first item.

@item
This is the second item.
@end enumerate


@node Index
@unnumbered Index

@printindex cp

@bye"))
    (let ((buffer (find-file-noselect temp-file)))
      (with-current-buffer buffer
        (makeinfo-buffer)
        (display-buffer)))))

(defun slime-info/display-info-buffer (texinfo-source)
  (let ((temp-file (make-temp-file "info-buffer-test")))
    (with-temp-file temp-file
      (insert texinfo-source))
    (let ((buffer (find-file-noselect temp-file)))
      (with-current-buffer buffer
        (makeinfo-buffer)
        (display-buffer)))))

(defun slime-info-package (package-name)
  "Show information about Common Lisp package named PACKAGE-NAME, using an Info buffer."
  (interactive (list (slime-read-symbol-name "Package name: ")))
  (when (not package-name)
    (error "No package name given"))
  (let ((texinfo-source (slime-eval `(swank:texinfo-source-for-package ',package-name))))
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
