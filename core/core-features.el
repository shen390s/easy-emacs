;;

(require 'cl-lib)
(require 'subr-x)

(defvar all-scope (make-hash-table)
  "All defined feature scope")

(cl-defstruct xfeature-scope name enter-hooks leave-hooks xfeatures)

(defun enable-xfeature (scope feature)
  ;;(message "enable-xfeature %s %s\n" scope feature)
  (let ((xscope (gethash scope all-scope)))
    (if xscope
	(puthash scope (make-xfeature-scope :name scope
					    :xfeatures (cons feature
							     (xfeature-scope-xfeatures xscope))
					    :enter-hooks nil
					    :leave-hooks nil)
		 all-scope)
      (puthash scope (make-xfeature-scope :name scope
					  :xfeatures (list feature)
					  :enter-hooks nil
					  :leave-hooks nil)
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


(defmacro scope! (scope enter-hooks leave-hooks)
  (let ((xscope (gethash scope all-scope)))
    (if (not xscope)
	(puthash scope (make-xfeature-scope :name scope
					    :xfeatures nil
					    :enter-hooks enter-hooks
					    :leave-hooks leave-hooks)
		 all-scope)
      (puthash scope (make-xfeature-scope :name scope
					  :xfeatures (xfeature-scope-xfeatures xscope)
					  :enter-hooks enter-hooks
					  :leave-hooks leave-hooks)
	       all-scope))))
		 

(defun collect-lists (acc lists)
  (if (null lists)
      acc
    (collect-lists (append acc (car lists))
		   (cdr lists))))

(defun actived-features ()
  (delete-dups
   (collect-lists nil
		  (mapcar #'(lambda (xscope)
			      (xfeature-scope-xfeatures xscope))
			  (hash-table-values all-scope)))))

(defun enter-scope (scope)
  (let ((xscope (gethash scope all-scope)))
    (when xscope
      (let ((enter-hooks (xfeature-scope-enter-hooks xscope)))
	(cl-loop for hook in enter-hooks
		 do (run-hooks hook))))))

(provide 'core-features)
