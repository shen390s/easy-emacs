;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'core-lib)

(defvar all-scope (make-hash-table)
  "All defined feature scope")

(defvar current-scope nil
  "The current scope which will be activated")

(defvar feature-key-args '(:activate :deactivate)
  "Key args for feature")
  
(cl-defstruct xfeature-scope name
	      modes
	      xfeatures)

(eval-and-compile
  (defun scope-function (scope tag subtag)
    (intern (concat (symbol-name scope)
		    "-scope-"
		    (symbol-name tag)
		    (symbol-name subtag)))))

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

(defun add-xfeature-to-scope (scope xfeature)
  (let ((xscope (get-or-create-scope scope)))
    (setf (xfeature-scope-xfeatures xscope)
	  (append (xfeature-scope-xfeatures xscope)
		  (list xfeature)))))

(defun filt-key-args (collected-args keys args)
  (if (null args)
      (reverse collected-args)
    (let ((x (car args))
	  (remain (cdr args)))
      (cond
       ((member x keys)
	(if (null remain)
	    (reverse collected-args)
	  (filt-key-args collected-args keys (cdr remain))))
       (t (filt-key-args (push x collected-args) keys remain))))))

(defun make-use-xfeature (scope feature)
  ;; -feature to disable feature explicit
  (eval-and-compile
    (defun parse-feature-name (n)
      (let ((svalue (symbol-name n)))
	(if (string= (substring svalue 0 1) "-")
	    (cons t (intern (substring svalue 1)))
	  (cons nil n))))
  
    (defun extract-feature-name ()
      (if (consp feature)
	  (cons nil (car feature))
	(parse-feature-name feature)))

    (defun extract-feature-args ()
      (if (consp feature)
	  (filt-key-args nil feature-key-args
			 (cdr feature))
	nil))
  
  (defun extract-hook-action (tag subtag)
    (if (consp feature)
	(cl-getf (cl-getf (cdr feature) tag) subtag)
      nil)))
 
  (pcase (extract-feature-name)
    (`(,is-disabled . ,feature-name)
     `(when-bind! xfeature (gethash ',feature-name all-xfeatures)
		  (when (config-xfeature xfeature)
		    (if ,is-disabled
			(progn
			  (add-hook (scope-function ',scope 'hook :after)
				    `(lambda ()
				       (when-call! ,(xfeature-off-fn xfeature)))))
		      (add-xfeature-to-scope ',scope
					     (list ',feature-name
						   `(lambda ()
						      ,@',(extract-hook-action :activate :pre)
						      (when-call! ,(xfeature-on-fn xfeature)
								  ,@',(extract-feature-args))
						      ,@',(extract-hook-action :activate :post))
						   `(lambda ()
						      ,@',(extract-hook-action :deactivate :pre)
						      (when-call! ,(xfeature-off-fn xfeature))
						      ,@',(extract-hook-action :deactivate :post))))))))))


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
		  ;; (bind-major-map :keys ("M-m")
		  ;; 		  :evil-keys (",")
		  ;; 		  :evil-mode (normal motion visual))
		  
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



;; create global scope
;;
(defvar global-scope-hook nil
  "Hooks run when enter global scope")

(defun global-scope ()
  t)

(scope! global nil
	global-scope)

(provide 'core-features)
