;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)

(defvar all-scope (make-hash-table)
  "All defined feature scope")

(defvar current-scope nil
  "The current scope which will be activated")

(cl-defstruct xfeature-scope name
	      enter-hooks
	      leave-hooks
	      after-setup
	      xfeatures)

(defun get-or-create-scope (scope)
  (let ((xscope (gethash scope all-scope)))
    (if (not xscope)
	(let ((xscope (make-xfeature-scope :name scope
					   :xfeatures nil
					   :enter-hooks nil
					   :leave-hooks nil
					   :after-setup nil)))
	  (progn
	    (puthash scope xscope all-scope)
	    xscope))
      xscope)))

(defun add-xfeature-to-scope (xscope xfeature)
  (setf (xfeature-scope-xfeatures xscope)
	(append (xfeature-scope-xfeatures xscope)
		(list xfeature))))

(defun make-use-xfeature (scope feature)
  (defun extract-feature-name ()
    (if (listp feature)
	(car feature)
      feature))
  (defun extract-before-activation ()
    (if (listp feature)
	(nth-car 1 (nth-car 2 feature))
      nil))

  (defun extract-after-activation ()
    (if (listp feature)
	(nth-car 2 (nth-car 2 feature))
      nil))

  (defun extract-before-deactivation ()
    (if (listp feature)
	(nth-car 1 (nth-car 3 feature))
      nil))

  (defun extract-after-deactivation ()
    (if (listp feature)
	(nth-car 2 (nth-car 3 feature))
      nil))
  
  (let ((feature-name (extract-feature-name)))
    `(let ((xfeature (gethash ',feature-name all-xfeatures)))
       (when xfeature
	 (let ((feature-on (xfeature-on-fn  xfeature))
	       (feature-off (xfeature-off-fn xfeature))
	       (config-ok (config-xfeature xfeature))
	       (xscope (get-or-create-scope ',scope)))
	   (add-xfeature-to-scope xscope
				  (list ',feature-name
					(lambda ()
					  (when config-ok
					    ,@(extract-before-activation)
					    (when feature-on
					      (funcall feature-on))
					    ,@(extract-after-activation)))
					(lambda ()
					  (when config-ok
					    ,@(extract-before-deactivation)
					    (when feature-off
					      (funcall feature-off))
					    ,@(extract-after-deactivation))))))))))

(defun conflict-feature (scope feature)
  (member scope (feature-enabled feature)))

(defun conflict-features (scope &rest features)
  (let ((conflicts 
	 (cl-loop for feature in features
		  collect (conflict-feature scope feature))))
    (cl-reduce #'or conflicts)))

;; Enable features in scope
;; (enable! scope feature1
;;                (feature2 ((code before activation) (code after activation))
;;                          ((code before deactivation) (code after deactivation))
;;                 ...)
(defmacro enable! (scope features)
  `(progn
     (let ((current-scope ',scope))
       ,@(cl-loop for feature in features
		  collect (make-use-xfeature scope feature)))))

;; Define a new scope
;; (scope! scope (hooks to be called when enter scope) (hooks to be call when leave scope))
(defmacro scope! (scope enter-hooks leave-hooks &rest after-setup)
  `(let ((xscope (get-or-create-scope ',scope)))
     (progn
       (setf (xfeature-scope-enter-hooks xscope) ',enter-hooks)
       (setf (xfeature-scope-leave-hooks xscope) ',leave-hooks)
       (setf (xfeature-scope-after-setup xscope) (lambda ()
						   ,@after-setup)))))

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
	(progn
	  (cl-loop for active-fn in (mapcar #'(lambda (x)
						(car (cdr x)))
					    (xfeature-scope-xfeatures xscope))
		   do (when active-fn
			(funcall active-fn)))
	  (funcall (xfeature-scope-after-setup xscope)))))))

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
	       do (progn
		    (add-hook hook
		    	      #'(lambda ()
		    		  (enter-scope scope)))))
      (cl-loop for hook in leave-hooks
	       do (progn
		    (add-hook hook
			      #'(lambda ()
				  (leave-scope scope))))))))

(defun build-hooks ()
  (maphash #'build-scope-hooks all-scope))

;; create global scope
;;
(defvar global-scope-hook nil
  "Hooks run when enter global scope")

(scope! global
	(global-scope-hook)
	nil
	(unless (member 'global
			(feature-enabled 'eldoc))
	  (global-eldoc-mode -1)))

(provide 'core-features)
