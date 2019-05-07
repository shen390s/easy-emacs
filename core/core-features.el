;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)

(defvar all-scope (make-hash-table)
  "All defined feature scope")

(defvar current-scope nil
  "The current scope which will be activated")

(cl-defstruct xfeature-scope name enter-hooks leave-hooks xfeatures)

(defun get-or-create-scope (scope)
  (let ((xscope (gethash scope all-scope)))
    (if (not xscope)
	(let ((xscope (make-xfeature-scope :name scope
					   :xfeatures nil
					   :enter-hooks nil
					   :leave-hooks nil)))
	  (progn
	    (puthash scope xscope all-scope)
	    xscope))
      xscope)))

(defun add-xfeature-to-scope (xscope xfeature)
  (setf (xfeature-scope-xfeatures xscope)
	(append (xfeature-scope-xfeatures xscope)
		(list xfeature))))

(defun build-activation (activation)
  (cl-loop for item in activation
	   collect `(setq ,(car item) ,(cdr item))))

(defun make-scope-xfeature (feature)
  (defun extract-feature-name ()
    (if (listp feature)
	(car feature)
      feature))

  (defun extract-before-activation ()
    (if (listp feature)
	(let ((activation (cdr feature)))
	  (if activation
	      (car activation)
	    nil))
      nil))

  (defun extract-after-activation ()
    (if (listp feature)
	(let ((activation (cdr (cdr feature))))
	  (if activation
	      (car activation)
	    nil))
      nil))

  (let ((feature-name (extract-feature-name))
	(before-activation (extract-before-activation))
	(after-activation (extract-after-activation)))
    (let ((xfeature (gethash feature-name all-xfeatures)))
      (when xfeature
	(let ((feature-on (xfeature-on-fn xfeature))
	      (feature-off (xfeature-off-fn xfeature)))
	  (list feature-name
		(if feature-on
		    `(lambda ()
		       ,@(build-activation before-activation)
		       (,feature-on))
		  `(lambda ()
		     ,@(build-activation before-activation)
		     nil))
		(if feature-off
		    `(lambda ()
		       (,feature-off)
		       ,@(build-activation after-activation))
		  `(lambda ()
		     t
		     ,@(build-activation after-activation)))))))))

;; Enable features in scope
;; (enable! scope feature1 (feature2 (preactive config) (post active config)) ...)
(defmacro enable! (scope features)
  (let ((xscope (get-or-create-scope scope)))
    (cl-loop for feature in features
	     do (add-xfeature-to-scope
		 xscope
		 (make-scope-xfeature feature)))))

;; Define a new scope
;; (scope! scope (hooks to be called when enter scope) (hooks to be call when leave scope))
(defmacro scope! (scope enter-hooks leave-hooks)
  (let ((xscope (get-or-create-scope scope)))
    (progn
      (setf (xfeature-scope-enter-hooks xscope) enter-hooks)
      (setf (xfeature-scope-leave-hooks xscope) leave-hooks))))

;; Return a list of scopes when the feature has been activated
(defun feature-enabled (feature)
  (let ((enabled-scope
	 (cl-loop for scope in (hash-table-keys all-scope)
		  collect (let ((features-in-scope
				 (mapcar #'car
					 (xfeature-scope-xfeatures
					  (gethash scope all-scope)))))
			    (when (member feature features-in-scope)
			      scope)))))
    (delq nil enabled-scope)))

;; All enabled features
(defun actived-features ()
  (delete-dups
   (collect-lists nil
		  (mapcar #'(lambda (xscope)
			      (mapcar #'car
				      (xfeature-scope-xfeatures xscope)))
			  (hash-table-values all-scope)))))

(defun enter-scope (scope)
  (let ((current-scope scope))
    (let ((xscope (gethash scope all-scope)))
      (when xscope
	(cl-loop for active-fn in (mapcar #'(lambda (x)
					      (car (cdr x)))
					  (xfeature-scope-xfeatures xscope))
		 do (when active-fn
		      (funcall active-fn)))))))

(defun leave-scope (scope)
  (let ((current-scope scope))
    (let ((xscope (gethash scope all-scope)))
      (when xscope
	(cl-loop for leave-fn in (mapcar #'(lambda (x)
					      (car (cdr (cdr x))))
					 (xfeature-scope-xfeatures xscope))
		 do (when leave-fn
		      (funcall leave-fn)))))))

(defun build-scope-hooks (scope xscope)
  (let ((enter-hooks (xfeature-scope-enter-hooks xscope))
	(leave-hooks (xfeature-scope-leave-hooks xscope)))
    (progn
      (cl-loop for hook in enter-hooks
	       do (add-hook hook
			    #'(lambda ()
				(enter-scope scope)))
      (cl-loop for hook in leave-hooks
	       do (add-hook hook #'(lambda ()
				     (leave-scope scope))))))))

(defun build-hooks ()
  (maphash #'build-scope-hooks all-scope))

;; create global scope
;;
(defvar global-scope-hook nil
  "Hooks run when enter global scope")

(scope! global (global-scope-hook) nil)

(provide 'core-features)
