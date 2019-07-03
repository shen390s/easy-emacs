;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)

(defvar all-scope (make-hash-table)
  "All defined feature scope")

(defvar current-scope nil
  "The current scope which will be activated")

(cl-defstruct xfeature-scope name
	      modes
	      xfeatures)

(defun scope-function (scope tag subtag)
  (intern (concat (symbol-name scope)
		  "-scope-"
		  (symbol-name tag)
		  (symbol-name subtag))))

(defun get-or-create-scope (scope)
  (let ((xscope (gethash scope all-scope)))
    (if (not xscope)
	(let ((xscope (make-xfeature-scope :name scope
					   :xfeatures nil
					   :modes nil)))
	  (progn
	    (puthash scope xscope all-scope)
	    xscope))
      xscope)))

(defun add-xfeature-to-scope (xscope xfeature)
  (setf (xfeature-scope-xfeatures xscope)
	(append (xfeature-scope-xfeatures xscope)
		(list xfeature))))

(defun make-use-xfeature (scope feature)
  ;; -feature to disable feature explicit
  (defun parse-feature-name (n)
    (let ((svalue (symbol-name n)))
      (if (string= (substring svalue 0 1) "-")
	  (cons t (intern (substring svalue 1)))
	(cons nil n))))
  
  (defun extract-feature-name ()
    (if (listp feature)
	(cons nil (car feature))
      (parse-feature-name feature)))
  
  (defun extract-hook-action (tag subtag)
    (if (listp feature)
	(cl-getf (cl-getf (cdr feature) tag) subtag)
      nil))
 
  (let ((feature-value (extract-feature-name)))
    (let ((feature-name (cdr feature-value))
	  (is-disabled (car feature-value)))
      `(when-bind! xfeature (gethash ',feature-name all-xfeatures)
		   (let ((xscope (get-or-create-scope ',scope)))
		     (if ,is-disabled
			 (let ((feature-off (xfeature-off-fn xfeature)))
			   (add-hook (scope-function ',scope 'hook :after)
				     (lambda ()
				       (when feature-off
					 (funcall feature-off)))))
		       (let ((feature-on (xfeature-on-fn  xfeature))
			     (feature-off (xfeature-off-fn xfeature))
			     (config-ok (config-xfeature xfeature)))
			 (add-xfeature-to-scope xscope
						(list ',feature-name
						      (lambda ()
							(when config-ok
							  ,@(extract-hook-action :activate :pre)
							  (when feature-on
							    (funcall feature-on))
							  ,@(extract-hook-action :activate :post)))
						      (lambda ()
							(when config-ok
							  ,@(extract-hook-action :deactivate :pre)
							  (when feature-off
							    (funcall feature-off))
							  ,@(extract-hook-action :deactivate :post))))))))))))

(defun conflict-feature (scope feature)
  (member scope (feature-enabled feature)))

(defun conflict-features (scope &rest features)
  (let ((conflicts 
	 (cl-loop for feature in features
		  collect (conflict-feature scope feature))))
    (cl-reduce #'or conflicts)))

;; Enable features in scope
;; (enable! scope feature1
;;                (feature2 :activate (:pre (code before activation) :post (code after activation))
;;                          :deactivate (:pre (code before deactivation) :post (code after deactivation))
;;                 ...)
(defmacro enable! (scope features)
  `(progn
     (let ((current-scope ',scope))
       ,@(cl-loop for feature in features
		  collect (make-use-xfeature scope feature)))))

(defvar nest-scope nil
  "Nested scopes")
(defvar nest-level 0
  "Nest level of scope")

(defmacro incr! (var val)
  `(setq ,var (+ ,var ,val)))

;; Define a new scope
;; (scope! scope (hooks to be called when enter scope) (hooks to be call when leave scope))
(defmacro scope! (scope parent &rest modes)
  `(progn
     (defvar ,(scope-function scope 'hook :before) nil)
     (defvar ,(scope-function scope 'hook :after) nil)
     (defun ,(scope-function scope 'entry :main) (origin-fun &rest args)
       (run-hooks ',(scope-function scope 'hook :before))
       (when (= nest-level 0)
	 (push ',scope nest-scope))
       (incr! nest-level 1)
       (when ',parent
	 (push ',parent nest-scope))

       (let ((res (apply origin-fun args)))
	 (activate-scope ',scope)
	 (incr! nest-level -1)
	 (when (= nest-level 0)
	   (cl-loop for n in (reverse nest-scope)
		    do (run-hooks (scope-function n 'hook :after)))
	   (setq nest-scope nil))
	 res))
     ,@(cl-loop for mode in modes
		collect `(add-hook 'easy-emacs-boot-done-hook
				   (lambda ()
				     (advice-add ',mode
						 :around
						 #',(scope-function scope 'entry :main)))))))

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

(defun activate-scope (scope)
  (let ((current-scope scope))
    (when-bind! xscope (gethash scope all-scope)
		(progn
		  (bind-major-map :keys ("M-m")
				  :evil-keys (",")
				  :evil-mode (normal motion visual))
		  
		  (cl-loop for active-fn in (mapcar #'(lambda (x)
							(second x))
						    (xfeature-scope-xfeatures xscope))
			   do (when active-fn
				(funcall active-fn)))))))

(defun deactivate-scope (scope)
  (let ((current-scope scope))
    (when-bind! xscope (gethash scope all-scope)
		(cl-loop for leave-fn in (mapcar #'(lambda (x)
						     (third x))
						 (xfeature-scope-xfeatures xscope))
			 do (when leave-fn
			      (funcall leave-fn))))))


(defun enter-global ()
  (easy-emacs-boot-done)
  (global-scope))

;; create global scope
;;
(defvar global-scope-hook nil
  "Hooks run when enter global scope")

(defun global-scope ()
  t)

(scope! global nil
	global-scope)

;; Actions to be done after we enter global scope
;;
(defun after-enter-global ()
  (load custom-file t t))

(add-hook (scope-function  'global 'hook :after)
          'after-enter-global)

(provide 'core-features)
