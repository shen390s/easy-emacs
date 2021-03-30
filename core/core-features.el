;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'core-lib)
(require 'core-log)
(require 'core-modules)

(defvar feature-key-args '(:activate :deactivate :when)
  "Key args for feature")
  
(defvar actived-modes nil
  "A list of actived modes")

(defvar mode-scope-alist nil
  "assoc list of mode and scope")

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

(defun extract-active-condition (feature)
  (if (consp feature)
      (cl-getf (cdr feature) :when t)
    t))

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
	   (active-condition (extract-active-condition feature))
	   (args (extract-feature-args feature)))
       (list :name feature-name
	     :disabled is-disabled
	     :active-condition active-condition
	     :pre-activate-action pre-activate-action
	     :post-activate-action post-activate-action
	     :pre-deactivate-action pre-deactivate-action
	     :post-deactivate-action post-deactivate-action
	     :args args)))))

(defun mk-function-call (fn args)
  (if fn
      `(,fn ,@args)
    `(dummy-fn)))

(defun mk-activate (condition pre-action fn post-action args)
  `(when ,condition
     ,@(mk-action pre-action)
     (let ((result ,(mk-function-call fn args)))
       ,@(mk-action post-action)
       result)))

(defun get-feature-activate-fn (feature)
  (let ((xfeature (get-feature feature)))
    (let ((fn (oref xfeature on-fn)))
      (if fn
	  fn
	#'dummy-fn))))

(defun get-feature-deactivate-fn (feature)
  (let ((xfeature (get-feature feature)))
    (let ((fn (oref xfeature off-fn)))
      (if fn
	  fn
	#'dummy-fn))))

(defun activate-feature (feature)
  (DEBUG! "activate feature %s"
	  feature)
  (mk-activate (plist-get feature :active-condition)
	       (plist-get feature :pre-activate-action)
	       (get-feature-activate-fn (plist-get feature :name))
	       (plist-get feature :post-activate-action)
	       (plist-get feature :args)))

(defun disable-feature (feature)
  (mk-activate t
	       nil
	       (get-feature-deactivate-fn (plist-get feature :name))
	       nil
	       nil))

(defun deactivate-feature (feature)
  (mk-activate  (plist-get feature :active-condition)
	        (plist-get feature :pre-deactivate-action)	
	        (get-feature-activate-fn (plist-get feature :name))
	        (plist-get feature :post-deactivate-action)
	        nil))

(eval-and-compile
  (defun add-feature-to-scope (scope feature)
    (DEBUG! "add feature %s to scope %s"
	    feature scope)
    
    (let ((zfeature (get-feature (plist-get feature :name))))
      (let ((current-scope scope))
	(when (and zfeature
		   (Feature/configure zfeature))
	  (Scope/add-feature (get-scope scope) feature))))))

(defun make-use-xfeature (scope feature)
  (DEBUG! "make-use-xfeature %s scope %s"
  	  feature scope)
 
  (let ((zfeature (parse-feature feature)))
    (DEBUG! "make-use-xfeature zfeature %s"
	    zfeature)
    (add-feature-to-scope scope
			  zfeature)))

(defun conflict-feature (scope feature)
  (member scope (feature-enabled-scopes feature)))

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
     ,(cl-loop for feature in features
	       do (make-use-xfeature scope feature))
     
     (defun ,(intern (format "%s-scope-enable-features" scope)) ()
       (DEBUG! "enable features for scope %s"
	       ',scope)
       ,@(cl-loop for feature in (oref (get-scope scope) features)
                 collect (unless (plist-get feature :disabled)
                           ;; call activate feature
                           (activate-feature feature))))

     (defun ,(intern (format "%s-scope-disable-features" scope)) ()
       (DEBUG! "disable features for scope %s"
	       ',scope)
       ,@(cl-loop for feature in (oref (get-scope scope) features)
                 collect (when (plist-get feature :disabled)
                           ;; call disable feature
                           (disable-feature feature))))

     (defun ,(intern (format "%s-scope-deactivate-features" scope)) ()
       (DEBUG! "deactivate features of scope %s"
	       scope)
       ,@(cl-loop for feature in (oref (get-scope scope) features)
                 collect (unless (plist-get feature :disabled)
                           (deactivate-feature feature))))))

;; return features of scope
(defun features-in-scope (scope)
  (if scope
      (let ((my-features
	     (mapcar #'(lambda (feature)
			 (plist-get feature :name))
		     (oref (gethash scope all-scopes)
			   features))))
	my-features)
    (progn
      (debug)
      (message "Warning: nil scope name passed to features-in-scope")
      nil)))

;; Return a list of scopes when the feature has been activated
(defun feature-enabled-scopes (feature)
  (let ((enabled-scope
	 (cl-loop for scope in (hash-table-keys all-scopes)
		  collect (let ((my-features (features-in-scope scope)))
			    (when (member feature my-features)
			      scope)))))
    (delq nil enabled-scope)))

(defun feature-in-scope (feature &optional scope)
  (let ((my-features (features-in-scope
		      (if scope
			  scope
			current-scope))))
    (member feature my-features)))

(defun feature-enabled (feature)
  (or (feature-in-scope feature current-scope)
      (feature-in-scope feature 'global)))

;; All enabled features
(defun actived-features ()
  (let ((mode-features (cl-remove-if-not (lambda (m)
					   (get-mode m))
					 actived-modes)))
    (delete-dups
     (collect-lists mode-features
		    (mapcar #'(lambda (scope)
				(mapcar #'(lambda (f)
                                             (plist-get f :name))
					(oref scope features)))
			    (hash-table-values all-scopes))))))

(defun enter-scope-prepare (scope-name)
  (DEBUG! "Prepare to enter scope %s mode %s"
	  scope-name major-mode)
  (when scope-name
    (setq current-scope scope-name)
    (install-packages-for-scope scope-name)
    (let ((scope (get-scope scope-name)))
      (when scope
	(Scope/pre-activate scope)))))

(defun enter-scope-final (scope-name)
  (DEBUG! "Activate scope %s mode %s"
	  scope-name major-mode)
  (when scope-name
    (let ((scope (get-scope scope-name)))
      (when scope
	(Scope/activate scope)
	(Scope/post-activate scope)))))

(defun enter-scope (scope entry args)
  (DEBUG! "Entering scope %s args %s"
	  scope args)
  (enter-scope-prepare scope)
  (let ((res (if entry
		 (apply entry args)
	       t)))
    (enter-scope-final scope)
    res))
  

(defun config-mode (m)
  (let ((zmode (get-mode m)))
    (if zmode
	(Feature/configure zmode)
      t)))

(defmacro attach! (scope &rest modes)
  `(progn
     (setq current-scope ',scope)
     ,@(cl-loop for mode in modes
		collect `(when (config-mode ',mode)
			   (setf mode-scope-alist
			       (plist-put mode-scope-alist
					  ',(local-or-rmode-name mode)
					  ',scope))))))

(defun mode-scope (mode)
  (plist-get mode-scope-alist mode))

;; create global scope
;;

(defun prepare-scope ()
  (let ((scope (mode-scope major-mode)))
    (enter-scope-prepare scope)))

(defun turn-on-scope ()
  (let ((scope (mode-scope major-mode)))
    (enter-scope-final scope)))

(defun turn-off-scope ()
  (let ((scope-name (mode-scope major-mode)))
    (DEBUG! "Leaving scope %s mode %s"
	    scope-name major-mode)
    (let ((scope (get-scope scope-name)))
        (when scope
            (Scope/deactivate scope)))))

(defun global-scope ()
  (add-hook 'change-major-mode-after-body-hook
	    #'prepare-scope)
  (add-hook 'after-change-major-mode-hook
	    #'turn-on-scope)
  (add-hook 'change-major-mode-hook
	    #'turn-off-scope)
  t)

(scope! global)

(defun enter-global ()
  (when (fboundp 'easy-emacs-boot-done)
    (let ((global-scope (get-scope 'global)))
      (Scope/add-hook global-scope
		      'before
		      #'easy-emacs-boot-done)))
  (enter-scope 'global #'global-scope nil))

(provide 'core-features)
