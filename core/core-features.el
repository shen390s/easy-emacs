;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'core-lib)
(require 'core-log)
(require 'core-modules)

(defvar all-scopes (make-hash-table)
  "All defined feature scope")

(defvar actived-scopes nil
  "All activated scopes")

(defvar current-scope nil
  "The current scope which will be activated")

(defvar feature-key-args '(:activate :deactivate)
  "Key args for feature")
  
(cl-defstruct xfeature-scope
  name  ;; Name of scope
  modes ;; modes which have been attached
  xfeatures ;; features which have been enabled in scope
  )

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
  (let ((xscope (gethash scope all-scopes)))
    (if (not xscope)
	(let ((xscope (make-xfeature-scope :name scope
					   :xfeatures nil
					   :modes nil)))
	  (progn
	    (puthash scope xscope all-scopes)
	    xscope))
      xscope)))

(defun add-xfeature-to-scope (scope xfeature)
  (DEBUG! "add xfeature %s to scope %s" xfeature scope)
  (let ((xscope (get-or-create-scope scope)))
    (setf (xfeature-scope-xfeatures xscope)
	  (append (xfeature-scope-xfeatures xscope)
		  (list xfeature)))))

(defun attach-mode-to-scope (scope mode)
  (DEBUG! "attach mode %s to scope %s" mode scope)
  (let ((xscope (get-or-create-scope scope)))
    (push (get-xmode mode) (xfeature-scope-modes xscope))))

(defun xfeature-scope-mode-pkgs (scope)
  (let ((zpkgs nil))
    (when (fboundp 'mode-pkgs)
      (let ((xscope (get-or-create-scope scope)))
	(progn
	  (cl-loop for mode in (xfeature-scope-modes xscope)
		   do (cl-loop for pkg in (mode-pkgs mode)
			       do (push pkg zpkgs))))))
    (DEBUG! "mode packages for scope %s: %s"
	    scope zpkgs)
    (delete-dups zpkgs)))

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
  (DEBUG! "make-use-xfeature %s scope %s"
	  feature scope)
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
		  (DEBUG! "when-bind! xfeature %s is-disabled %s" xfeature ,is-disabled)
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
(defmacro scope! (scope parent &optional config-fn)
  `(progn
     (defvar ,(scope-function scope 'hook :before) nil
       "Hooks run before scope ,scope has been activated")
     
     (defvar ,(scope-function scope 'hook :after) nil
       "Hooks run after scope ,scope has been activated")
     
     (defvar ,(scope-function scope 'var :pkg-installed) nil
       "Var to tell us whether the packages of scope has been installed")

     (defvar ,(scope-function scope 'var :config-fn) #',config-fn
       "Var to hold scope configuration function")
     
     (defun ,(scope-function scope 'entry :install-pkgs) ()
       (unless ,(scope-function scope 'var :pkg-installed)
	 (DEBUG! "Installing packages for scope %s"
		 ',scope)
	 (,(scope-function parent 'entry :install-pkgs))
	 (when (fboundp 'actived-packages)
	   (let ((xscope (gethash ',scope all-scopes)))
	     (when xscope
	       (cl-loop for pkg in
			(append (xfeature-scope-mode-pkgs ',scope)
				(actived-packages
				 (mapcar #'car
					 (xfeature-scope-xfeatures xscope))))
			do (install-package-by-name pkg)))))
	   (setf ,(scope-function scope 'var :pkg-installed) t)))

     (defun ,(scope-function scope 'entry :pre-activate) ()
       (,(scope-function parent 'entry :pre-activate))
       (run-hooks ',(scope-function scope 'hook :before)))

     (defun ,(scope-function scope 'entry :post-activate) ()
       (run-hooks ',(scope-function scope 'hook :after))
       (,(scope-function parent 'entry :post-activate)))

     (defun ,(scope-function scope 'entry :activate)()
       (,(scope-function parent 'entry :activate))
       (activate-scope ',scope))

     (defun ,(scope-function scope 'entry :configure) ()
       (DEBUG! "trying to configure scope %s"
	       ',scope)
       (when (,(scope-function parent 'entry :configure))
	 (let ((config-fn ,(scope-function scope 'var :config-fn)))
	   (DEBUG! "configure scope %s" ',scope)
	   (if config-fn
	       (funcall config-fn)
	     t))))

     (defun ,(scope-function scope 'entry :prepare) ()
       (,(scope-function scope 'entry :install-pkgs))
       (,(scope-function scope 'entry :configure))
       (when (member ',scope actived-scopes)
	 (let ((xscope (gethash ',scope all-scopes)))
	   (when xscope
	     (cl-loop for mode in (xfeature-scope-modes xscope)
		      (advice-add (xmode-entry mode)
				  :around
				  #',(mode-function (xmode-name mode)))))))))
     
(defmacro activate! (&rest scopes)
  `(progn
     (setf actived-scopes ',scopes)
     ;; enable global and elisp scope as default
     (unless (member 'global actived-scopes)
       (push 'global actived-scopes))
     (unless (member 'elisp actived-scopes)
       (push 'elisp actived-scopes))
     (DEBUG! "scopes to be activated: %s"
	     actived-scopes)
     ,@(cl-loop for scope in actived-scopes
		collect `(,(scope-function scope 'entry :configure)))))

;; Return a list of scopes when the feature has been activated
(defun feature-enabled (feature)
  (let ((enabled-scope
	 (cl-loop for scope in actived-scopes
		  collect (let ((features-in-scope
				 (mapcar #'car
					 (xfeature-scope-xfeatures
					  (gethash scope all-scopes)))))
			    (when (member feature features-in-scope)
			      scope)))))
    (delq nil enabled-scope)))

(defun activate-scope (scope)
  (let ((current-scope scope))
    (when-bind! xscope (gethash scope all-scopes)
		(progn
		  ;; (bind-major-map :keys ("M-m")
		  ;; 		  :evil-keys (",")
		  ;; 		  :evil-mode (normal motion visual))
		  
		  (cl-loop for active-fn in (mapcar #'(lambda (x)
							(second x))
						    (xfeature-scope-xfeatures xscope))
			   do (when active-fn
				(condition-case err
				    (funcall active-fn)
				  (error (WARN! "%s" (error-message-string err))))))))))

(defun deactivate-scope (scope)
  (let ((current-scope scope))
    (when-bind! xscope (gethash scope all-scopes)
		(cl-loop for leave-fn in (mapcar #'(lambda (x)
						     (third x))
						 (xfeature-scope-xfeatures xscope))
			 do (when leave-fn
			      (funcall leave-fn))))))


(defun install-packages-for-scope (scope)
  (let ((pkg-install-fn (scope-function scope 'entry :install-pkgs)))
    (when (fboundp pkg-install-fn)
      (funcall pkg-install-fn))))

;; All enabled features
(defun actived-features ()
  (delete-dups
   (collect-lists nil
                  (mapcar #'(lambda (scope)
                              (let ((xscope (gethash scope all-scopes)))
				(mapcar #'car
					(xfeature-scope-xfeatures xscope))))
                          actived-scopes))))

(defun enter-scope (scope entry args)
  (install-packages-for-scope scope)
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

(defmacro attach! (scope &rest modes)
  `(progn
     ,@(cl-loop for mode in modes
		do (attach-mode-to-scope scope mode))
     ,@(cl-loop for mode in modes
		collect `(defun ,(mode-function mode) (origin-fun &rest args)
			     (enter-scope ',scope origin-fun args)))))

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
