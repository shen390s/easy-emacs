;;

(require  'cl-lib)

(defvar all-scope (make-hash-table)
  "All defined feature scope")

(cl-defstruct xfeature-scope name xfeatures)

(defun enable-xfeature (scope feature)
  (message "enable-xfeature %s %s\n" scope feature)
  (let ((xscope (gethash scope all-scope)))
    (if xscope
	(puthash scope (make-xfeature-scope :name scope
					    :xfeatures (cons feature
							     (xfeature-scope-xfeatures xscope)))
		 all-scope)
      (puthash scope (make-xfeature-scope :name scope
					  :xfeatures (list feature))
	       all-scope))))

(defun enable-xfeatures (features)
  (cl-loop for feature in features
	   do (if (not (listp feature))
		  (enable-xfeature 'global feature)
		(let ((scope (car feature)))
		  (cl-loop for feature2 in (cdr feature)
			   do (enable-xfeature scope feature2))))))

(defmacro enable! (&rest features)
  (enable-xfeatures features))

(provide 'core-features)
