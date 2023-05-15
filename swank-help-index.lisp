(require :montezuma)

(in-package :swank-help)

;; Apropos using a Montezuma index

(defvar *fulltext-search-index* nil)

(defun initialize-fulltext-search-index ()
  (setf *fulltext-search-index* (make-instance 'montezuma:index
					       :analyzer (make-instance 'montezuma:whitespace-analyzer)))
  (dolist (package (list-all-packages))
    (do-external-symbols (symbol package)
      (let ((props
	      (def-properties:symbol-properties symbol :shallow)))
	(dolist (def props)
	  (montezuma:add-document-to-index *fulltext-search-index*
					   (list (cons "name" (symbol-name (cdr (assoc :name def))))
						 (cons "doc" (cdr (assoc :documentation def)))
						 (cons "package" (package-name package))
						 (cons "type" (symbol-name (cdr (assoc :type def)))))))))))

(defun ensure-fulltext-search-index ()
  (when (null *fulltext-search-index*)
    (initialize-fulltext-search-index)))

(defun apropos-fulltext-search-index (query &key external-only case-sensitive package (return 'symbol))
  (let (matches)
    (montezuma:search-each *fulltext-search-index* query
			   #'(lambda (doc-number score)
			       (let ((doc (montezuma:get-document *fulltext-search-index* doc-number)))
				 (let ((result (if (eql return 'symbol)
						   (intern
						    (montezuma:document-value doc "name")
						    (montezuma:document-value doc "package"))
						   ;; else
						   doc)))
						   
				 (push result matches)))))
    (nreverse matches)))
