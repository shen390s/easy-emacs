;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'core-lib)
(require 'core-log)
(require 'core-modules)

(defvar feature-key-args '(:activate :deactivate)
  "Key args for feature")
  
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

  ;; -feature to disable feature explicit
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
    nil))

(defun parse-feature (feature)
  (pcase (extract-feature-name feature)
    (`(,is-disabled . ,feature-name)
     (let ((pre-activate-action (extract-hook-action feature
						     :activate
						     :pre))
	   (post-activate-action (extract-hook-action feature
						      :activate
						      :post))
	   (pre-deactivate-action (extract-hook-action feature
						       :deactivate
						       :pre))
	   (post-deactivate-action (extract-hook-action feature
							:deactivate
							:post))
	   (args (extract-feature-args feature)))
       (list :name feature-name
	     :disabled is-disabled
	     :pre-activate-action pre-activate-action
	     :post-activate-action post-activate-action
	     :pre-deactivate-action pre-deactivate-action
	     :post-deactivate-action post-deactivate-action
	     :args args)))))

(eval-and-compile
  (defun dummy-fn ()
  t))

(defun mk-function-call (fn args)
  (if fn
      `(,fn ,@args)
    `(dummy-fn)))

(defun mk-pre-post-call (action)
  (if action
      `,@action
    `((dummy-fn))))

(defun mk-action (pre-action fn post-action args)
  `(progn
     ,@(mk-pre-post-call pre-action)
     (let ((result ,(mk-function-call fn args)))
       ,@(mk-pre-post-call post-action)
       result)))

(eval-and-compile
  (defun add-feature-to-scope (scope feature)
    (DEBUG! "add feature %s to scope %s"
	    feature scope)
    (let ((zfeature (get-feature (car feature))))
      (when (Feature/configure zfeature)
	(Scope/add-feature (get-scope scope) feature)))))

(defun make-use-xfeature (scope feature)
  (DEBUG! "make-use-xfeature %s scope %s"
  	  feature scope)
 
  (let ((zfeature (parse-feature feature)))
    (let ((name (plist-get zfeature :name))
	  (disabled (plist-get zfeature :disabled))
	  (pre-activate-action (plist-get zfeature :pre-activate-action))
	  (post-activate-action (plist-get zfeature :post-activate-action))
	  (pre-deactivate-action (plist-get zfeature :pre-deactivate-action))
	  (post-deactivate-action (plist-get zfeature :post-deactivate-action))
	  (args (plist-get zfeature :args)))
      (let ((fn-deactive1 (scope-function scope	name
					  :deactive-no-hook))
	    (fn-activate (scope-function scope  name
					 :activate))
	    (fn-deactivate (scope-function scope name
					   :deactivate)))
	(when-bind! xfeature (get-feature name)
		    (if disabled
			`(progn
			   (defun ,fn-deactive1 ()
			     ,(mk-action nil (oref xfeature off-fn)
					 nil nil))
			   (add-hook ',(scope-function scope
						       :hook :after)
				     ',fn-deactive1))
		      `(progn
			 (defun ,fn-activate ()
			   ,(mk-action pre-activate-action
				       (oref xfeature on-fn)
				       post-activate-action
				       args))
			 (defun ,fn-deactivate ()
			   ,(mk-action pre-deactivate-action
				       (oref xfeature off-fn)
				       post-deactivate-action
				       nil))

			 (add-feature-to-scope ',scope
					       (list ',name
						     ',fn-activate
						     ',fn-deactivate)))))))))

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
     ,@(cl-loop for feature in features
		collect (let ((zcode (make-use-xfeature scope feature)))
			  (DEBUG2! "zcode %s" zcode)
			  zcode))))

;; Return a list of scopes when the feature has been activated
(defun feature-enabled (feature)
  (let ((enabled-scope
	 (cl-loop for scope in (hash-table-keys all-scopes)
		  collect (let ((features-in-scope
				 (mapcar #'car
					 (oref (gethash scope all-scopes) features))))
			    (when (member feature features-in-scope)
			      scope)))))
    (delq nil enabled-scope)))

;; All enabled features
(defun actived-features ()
  (delete-dups
   (collect-lists nil
                  (mapcar #'(lambda (xscope)
                              (mapcar #'car
                                      (oref xscope features)))
                          (hash-table-values all-scopes)))))

(defun enter-scope (scope entry args)
  (DEBUG! "Entering scope %s args %s"
	  scope args)
  (install-packages-for-scope scope)
  (funcall (scope-function scope 'entry :pre-activate))
  (let ((res (if entry
		 (apply entry args)
	       t)))
    (funcall (scope-function scope 'entry :activate))
    (funcall (scope-function scope 'entry :post-activate))
    res))
  

(defmacro attach! (scope &rest modes)
  `(progn
     ,@(cl-loop for mode in modes
		collect `(defun ,(mode-function mode)
			     (origin-fun &rest args)
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
  (when (fboundp 'easy-emacs-boot-done)
    (add-hook (scope-function 'global 'hook :before)
	      #'easy-emacs-boot-done))
  (enter-scope 'global #'global-scope nil))

(provide 'core-features)
