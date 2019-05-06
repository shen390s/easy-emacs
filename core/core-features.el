;;

(require 'cl-lib)
(require 'subr-x)

(defvar all-scope (make-hash-table)
  "All defined feature scope")

(defvar enabled-features nil
  "All enabled features")

(cl-defstruct xfeature-scope name enter-hooks leave-hooks xfeatures)

(defun enable-xfeature (scope feature)
  (let ((xscope (gethash scope all-scope)))
    (if xscope
	(puthash scope
		 (make-xfeature-scope :name scope
				      :xfeatures (append
						  (xfeature-scope-xfeatures xscope)
						  (list feature))
				      :enter-hooks (xfeature-scope-enter-hooks xscope)
				      :leave-hooks (xfeature-scope-leave-hooks xscope))
		 all-scope)
      (puthash scope
	       (make-xfeature-scope :name scope
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
  (progn
    (setf enabled-features features)
    (enable-xfeatures features)))

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
		 

(defun feature-enabled (feature)
  (let ((enabled-scope (cl-loop for scope in (hash-table-keys all-scope)
				collect (let ((features-in-scope (xfeature-scope-xfeatures (gethash scope all-scope))))
					  (when (member feature features-in-scope)
					    scope)))))
    (delq nil enabled-scope)))

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

(defun build-scope-hooks (scope xscope)
  (let ((enter-hooks (xfeature-scope-enter-hooks xscope))
	(leave-hooks (xfeature-scope-leave-hooks xscope)))
    (cl-loop for feature in (xfeature-scope-xfeatures xscope)
	     do (let ((xfeature (gethash feature all-xfeatures)))
		  (when xfeature
		    (let ((feature-on (xfeature-on-fn xfeature))
			  (feature-off (xfeature-off-fn xfeature)))
		      (progn
			(when feature-on
			  (cl-loop for hook in enter-hooks
				   do (add-hook hook feature-on)))
			(when feature-off
			  (cl-loop for hook in leave-hooks
				   do (add-hook hook feature-off))))))))))

(defun build-hooks ()
  (maphash #'build-scope-hooks all-scope))

;; create global scope
;;
(defvar global-scope-hook nil
  "Hooks run when enter global scope")

(scope! global (global-scope-hook) nil)

(provide 'core-features)
