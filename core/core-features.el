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
  (defun scope-null ()
    t)
  
  (defun scope-function (scope tag subtag)
    (if scope
	(intern (concat (symbol-name scope)
			"-scope-"
			(symbol-name tag)
			(symbol-name subtag)))
      (intern "scope-null"))))

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
  
    (defun extract-feature-name (feature)
      (if (consp feature)
	  (cons nil (car feature))
	(parse-feature-name feature)))

    (defun extract-feature-args (feature)
      (if (consp feature)
	  (filt-key-args nil feature-key-args
			 (cdr feature))
	nil))
  
  (defun extract-hook-action (feature tag subtag)
    (if (consp feature)
	(cl-getf (cl-getf (cdr feature) tag) subtag)
      nil)))
 
  (pcase (extract-feature-name feature)
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
						      ,@',(extract-hook-action feature :activate :pre)
						      (when-call! ,(xfeature-on-fn xfeature)
								  ,@',(extract-feature-args feature))
						      ,@',(extract-hook-action feature :activate :post))
						   `(lambda ()
						      ,@',(extract-hook-action feature :deactivate :pre)
						      (when-call! ,(xfeature-off-fn xfeature))
						      ,@',(extract-hook-action feature :deactivate :post))))))))))


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

;; Define a new scope
;; (scope! scope (hooks to be called when enter scope) (hooks to be call when leave scope))
(defmacro scope! (scope parent)
  `(progn
     (defvar ,(scope-function scope 'hook :before) nil
       "Hooks run before scope ,scope has been activated")
     
     (defvar ,(scope-function scope 'hook :after) nil
       "Hooks run after scope ,scope has been activated")
     
     (defun ,(scope-function scope 'entry :pre-activate) ()
       (,(scope-function parent 'entry :pre-activate))
       (run-hooks ',(scope-function scope 'hook :before)))

     (defun ,(scope-function scope 'entry :post-activate) ()
       (run-hooks ',(scope-function scope 'hook :after))
       (,(scope-function parent 'entry :post-activate)))

     (defun ,(scope-function scope 'entry :activate)()
       (,(scope-function parent 'entry :activate))
       (activate-scope ',scope))))
     
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


(defun enter-scope (scope entry args)
  (funcall (scope-function scope 'entry :pre-activate))
  (let ((res (if entry
		 (apply entry args)
	       t)))
    ;;(activate-scope scope)
    (funcall (scope-function scope 'entry :activate))
    (funcall (scope-function scope 'entry :post-activate))
    res))
  
(eval-and-compile
  (defun mode-function (mode)
    (intern (concat (symbol-name mode)
		    ":entry"))))

(defmacro mode! (scope &rest modes)
  `(progn
     ,@(cl-loop for mode in modes
		collect `(defun ,(mode-function mode) (origin-fun &rest args)
			     (enter-scope ',scope origin-fun args)))
     ,@(cl-loop for mode in modes
		collect `(add-hook 'easy-emacs-boot-done-hook
				   (lambda ()
				     (advice-add ',mode
						 :around
						 #',(mode-function mode)))))))

;; create global scope
;;
(defun global-scope ()
  t)

(scope! global nil)

(defun enter-global ()
  (add-hook (scope-function 'global 'hook :before)
  	    #'easy-emacs-boot-done)
  (enter-scope 'global #'global-scope nil))
	    

(provide 'core-features)
