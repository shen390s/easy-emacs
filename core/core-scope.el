;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'pp)
(require 'core-lib)
(require 'core-log)
(require 'core-hooks)
(require 'core-modules)

(defclass Base-Config ()
  ((name :initarg :name
	 :initform "default")
   (config :initarg :config
	   :initform nil)
   (fns :initarg :fns
	:initform nil)
   (pkgs :initarg :pkgs
	 :initform nil)
   (pkg-installed :initarg :pkg-installed
		  :initform nil))
  "Configuration in scope")

(cl-defmethod Config/Call ((config Base-Config) fn _scope)
  (with-slots (fns) config
    (let ((f (plist-get fns fn)))
      (when f
	(funcall f)))))

(cl-defmethod Config/make-init ((config Base-Config) _scope)
  (with-slots (name fns) config
    (cl-loop for hook in '(:before-activate :after-activate)
	     do (let ((fn (plist-get fns hook)))
		  (when fn
		    (add-hook (intern (format "%s-%s-hook"
					      name
					      (keyword-name hook)))
			      `,@fn))))))

(eval-when-compile
  (defun make-config-method (key name)
    (DEBUG! "make-config-method key %s name %s" (pp-to-string key) name)
    `(cl-defmethod ,(intern (format "Config/%s" name))
       ((config Base-Config) scope)
       (Config/Call config ,key scope))))

(defmacro make-config-methods! (methods)
  `(progn
     ,@(cl-loop for key in (filt-out-non-keywords methods)
		collect (make-config-method key (plist-get methods key)))))

(make-config-methods! (:pre-check Check:before
				  :check Check:primary
				  :after-check Check:after
				  :pre-prepare Prepare:before
				  :prepare Prepare:primary
				  :after-prepare Prepare:after
				  :before-activate Activate:before
				  :activate Activate
				  :after-activate Activate:after))

(cl-defmethod Config/Pkgs:get ((config Base-Config))
  (with-slots (pkgs) config
    pkgs))

(cl-defmethod Config/Pkgs:update ((_config Base-Config) _scope)
  t)

(defclass InitialSettings-Config (Base-Config)
  ()
  "for initial settings")

(cl-defmethod Config/make-init ((config InitialSettings-Config) _scope)
  (with-slots (config fns) config
    (setf fns (list :pre-prepare
		    `(lambda ()
		       ,@config))))
  (cl-call-next-method))

(defclass Mode-Config (Base-Config)
  ()
  "Configuration for modes")

(cl-defmethod Config/make-init ((config Mode-Config) _scope)
  (with-slots (fns config) config
    (setf fns (make-scope-help-fns (list :config #'mode-feature-config
					 :prepare #'mode-feature-prepare
					 :activate #'mode-feature-activate)
				   config)))
  (cl-call-next-method))

(cl-defmethod Config/Pkgs:update ((config Mode-Config) _scope)
  (with-slots (config pkgs) config
    (let ((mode (if (listp config)
		    (car config)
		  config))
	  (options (if (listp config)
		       (cdr config)
		     nil)))
      (DEBUG! "mode = %s options = %s"
	      mode options)
      (setf pkgs
	    (mode-feature-pkgs mode options)))))

(defclass App-Config (Base-Config)
  ((apps :initarg :apps
	 :initform nil))
  "Application Configuration")

(cl-defmethod Config/make-init ((config App-Config) scope)
  (with-slots (fns config) config
    (setf fns (make-scope-help-fns (list :config #'(lambda (app phase options)
						     (app-feature-config app scope
									 phase options))
					 :prepare #'(lambda (app phase options)
						      (app-feature-prepare app scope
									   phase options))
					 :activate #'(lambda (app phase options)
						       (app-feature-activate app scope
									     phase options)))
				   config)))
  (cl-call-next-method))

(cl-defmethod Config/Pkgs:update ((config App-Config) scope-name)
  (DEBUG! "update package requires for %s scope %s"
	  config scope-name)
  (with-slots (config pkgs) config
    (let ((app (if (listp config)
		   (car config)
		 config))
	  (options (if (listp config)
		       (cdr config)
		     nil)))
      (DEBUG! "app = %s options = %s" app options)
      (setf pkgs
	    (invoke-feature app 'pkglist 'app 'ignore options)))))


(defclass Scope ()
  ((name :initarg :name
	 :initform "null-scope")
   (configs :initarg :configs
	    :initform nil))
  "Class scope for EasyEmacs")

(cl-defmethod Object/to-string ((scope Scope))
  (format "scope %s" scope))

(defmacro with-scope! (scope-name scope-var &rest body)
  `(when ,scope-name
     (let ((,scope-var (get-scope ,scope-name)))
       (when ,scope-var
	 ,@body))))

(cl-defmethod Scope/name ((scope Scope))
  (with-slots (name) scope
    name))

(cl-defmethod Scope/add-config ((scope Scope) config)
  (with-slots (configs) scope
    (push config configs)))

(cl-defmethod Scope/get-pkgs ((scope Scope))
  (collect-lists nil
		 (with-slots (configs name) scope
		   (cl-loop for config in configs
			    collect (progn
				      (Config/Pkgs:update config name)
				      (Config/Pkgs:get config))))))

(eval-when-compile
  (defun make-scope-method (action phase)
    `(cl-defmethod ,(intern (format "Scope/%s:%s" action phase))
       ((scope Scope))
       (with-slots (name configs) scope
	 (cl-loop  for config in configs
		   do (,(intern (format "Config/%s:%s"
					(if (string= action "Configure")
					    "Check"
					  action)
					phase))
		       config name))))))

(defmacro make-scope-methods! ()
  (let ((actions '(Configure Prepare))
	(phases '(before primary after)))
    `(progn
       ,@(collect-lists nil
			(cl-loop for action in actions
				 collect (cl-loop for phase in phases
						  collect (make-scope-method action
									     phase)))))))
(make-scope-methods!)

(cl-defmethod Scope/Activate ((scope Scope))
  (DEBUG! "activate scope %s"
	  (pp-to-string scope))
  (with-slots (name configs) scope
    (cl-loop for config in configs
	     do (Config/Activate config name))))

(cl-defmethod Scope/install-pkgs ((scope Scope))
  (let ((pkg-installed (oref scope pkg-installed)))
    (unless pkg-installed
      (cl-loop for pkg in (Scope/get-pkgs scope)
	       do (install-package pkg))
      (setf (oref scope pkg-installed) t))))

(cl-defmethod Scope/make-config ((scope Scope) config)
  (let ((config-name (Scope/config-name scope config))
	(config-class (Scope/config-class scope)))
    (when config-class
      (with-slots (name) scope
	(make-config name
		     config-class
		     config-name
		     config)))))

(cl-defmethod Scope/config-name ((_scope Scope) config)
  (if (listp config)
      (if config
	  (car config)
	'Null)
    `,config))

(cl-defmethod Scope/config-class ((_scope Scope))
  nil)

(defclass Initial-Scope (Scope)
  ()
  "Scope for initialization")

(cl-defmethod Scope/config-name ((_scope Initial-Scope) _config)
  'init)

(cl-defmethod Scope/config-class ((_scope Initial-Scope))
  'InitialSettings-Config)

(defclass Core-Scope (Scope)
  ()
  "Core scope of easy-emacs")

(cl-defmethod Scope/config-class ((_scope Core-Scope))
  'App-Config)

(defclass Mode-Scope (Scope)
  ()
  "Scope for modes")

(cl-defmethod Scope/config-class ((_scope Mode-Scope))
  'Mode-Config)


(cl-defmethod Scope/add-config ((scope Mode-Scope) config)
  (cl-call-next-method)
  (let ((mode (intern (format "%s-mode" (oref config name)))))
    (let ((keybinds (plist-get (normalize-options (oref config config))
			       :keybinds)))
      (DEBUG! "Scope/add-config mode %s keybinds %s"
	      mode keybinds)
      (when keybinds
	(mk-mode-keybinds mode keybinds)))))

(defclass UI-Scope (Scope)
  ()
  "Scope for UI")

(cl-defmethod Scope/config-class ((_scope UI-Scope))
  'App-Config)

(defclass Completion-Scope (Scope)
  ()
  "Scope for completion")

(cl-defmethod Scope/config-class ((_scope Completion-Scope))
  'App-Config)

(defclass App-Scope (Scope)
  ()
  "Scope for Application")

(cl-defmethod Scope/config-class ((_scope App-Scope))
  'App-Config)

(defclass Editor-Scope (Scope)
  ()
  "Scope for editor related")

(cl-defmethod Scope/config-class ((_scope Editor-Scope))
  'App-Config)

(defvar all-scopes (make-hash-table)
  "All defined scopes")

(defun make-scope (scope-name &optional configs)
  (DEBUG! "make-scope %s %s"
	  scope-name configs)
  (let ((scope-class
	 (pcase scope-name
	   ('init 'Initial-Scope)
	   ('modes 'Mode-Scope)
	   ('ui 'UI-Scope)
	   ('completion 'Completion-Scope)
	   ('app 'App-Scope)
	   ('editor 'Editor-Scope)
	   ('core 'Core-Scope)
	   (_ nil))))
    (let ((scope (make-instance scope-class
				:name scope-name)))
      (cl-loop for c in configs
	       do (Scope/add-config scope c))
      (puthash scope-name scope all-scopes))))

(defmacro scope! (name &optional configs) 
  `(progn
     (make-scope ',name ',configs)))

(defun get-scope (name)
  (let ((scope (gethash name all-scopes)))
    (DEBUG2! "get-scope %s"
	     (Object/to-string scope))
    scope))

(defmacro foreach-scope! (name scope &rest body)
  `(maphash '(lambda (,name ,scope)
	       ,@body)
	    all-scopes))

(defun make-config (scope config name configs)
  (DEBUG! "make-config %s %s %s %s"
	  scope config name (pp-to-string configs))
  (let ((c (make-instance config
			  :name name
			  :config configs)))
    (Config/make-init c scope)
    c))

(defun merge-keybinds (c1 c2)
  (let ((keybinds (plist-get c1 :keybinds))
	(keybinds2 (plist-get c2 :keybinds)))
    (setq key (pop keybinds2))
    (while keybinds2
      (setq def (pop keybinds2))
      (unless (plist-get keybinds key)
	(setq keybinds
	      (plist-put keybinds
			 key def)))
      (setq key (pop keybinds2)))
    keybinds))

(defun merge-features (c1 c2)
  (let ((features (parse-features (plist-get c1 :features)))
	(features2 (parse-features (plist-get c2 :features))))
    (cl-loop for feature in features2
	     do (when (not (has-feature? features feature))
		  (setq features
			(append features (list feature)))))
    (unparse-features features)))

(defun inherit-config (c1 c2)
  (let ((m1 (intern (format "%s-mode" (car c1))))
	(m2 (intern (format "%s-mode" (car c2)))))
    (if (eq m1 m2)
	c1
      (if (derived-from? m1 m2)
	  (cons (car c1)
		(un-normalize-options
		 (let ((c1-n (normalize-options (cdr c1)))
		       (c2-n (normalize-options (cdr c2))))
		   (plist-put (plist-put c1-n
					 :keybinds
					 (merge-keybinds c1-n c2-n))
			      :features
			      (merge-features c1-n c2-n)))))
	c1))))

(defun fixup-mode-configs (configs)
  (DEBUG! "fixup-mode-configs = %s"
	  configs)
  (let ((sorted (sort configs #'(lambda (c1 c2)
				  (let ((m1 (intern (format "%s-mode" (car c1))))
					(m2 (intern (format "%s-mode" (car c2)))))
				    (if (derived-from? m1 m2)
					-1
				      (if (derived-from? m2 m1)
					  1
					0)))))))
    (cl-loop for config in sorted
	     collect (let ((c config))
		       (cl-loop for c1 in sorted
				do (setq c (inherit-config c c1)))
		       c))))

(defun config/:fixup-configs (scope-name configs)
  (DEBUG! "config/:fixup-configs scope %s configs %s"
	  scope-name configs)
  (let ((fixed-configs (pcase scope-name
			 ('modes (fixup-mode-configs configs))
			 (_ configs))))
    (DEBUG! "config/:fixup-configs scope %s  fixed = %s"
	    scope-name fixed-configs)
    fixed-configs))

(defun config/:make-scope (scope-name configs)
  `((DEBUG! "config/:make-%s %s"
	    ',scope-name ',(pp-to-string configs))
    (scope! ,(intern (symbol-name scope-name)))
    (with-scope! ',(intern (symbol-name scope-name)) scope
		 ,@(cl-loop for config in (config/:fixup-configs scope-name configs)
			    collect `(let ((c (Scope/make-config scope ',config)))
				       (when c
					 (setf (oref c config) ',config)
					 (Scope/add-config scope c)))))))

(defun mk-scope-handler-call (handler action app phase options)
  (let ((fn (plist-get handler action)))
    (if fn
	(pcase action
	  (:activate
	   `(:activate
	     (lambda ()
	       (,fn ',app ',phase ',options))))
	  (_ 
	   `(,phase (lambda ()
		      (,fn ',app ',phase ',options)))))
      `(,phase (lambda ()
		 t)))))

(defun make-config-hook (config hook)
  (DEBUG! "make-config-hook config %s hook %s"
	  (pp-to-string config) hook)
  (let ((hook-code (plist-get (collect-keyword-values config) hook)))
    (DEBUG! "make-config-hook code %s"
	    (pp-to-string hook-code))
    (if hook-code
	`(,hook (lambda ()
		  (progn
		    ,@hook-code)))
      nil)))

(defun make-config-hooks (config)
  (let ((hooks
	 (collect-lists nil
			(cl-loop for hook in (list :before-activate :after-activate)
				 collect (make-config-hook config
							   hook)))))
    (DEBUG! "make-config-hooks hooks %s config %s"
	    (pp-to-string hooks)
	    (pp-to-string config))
    hooks))

(defun make-scope-help-fns (scope-handler config)
  (DEBUG! "make-scope-help-fns handler %s config %s"
	  (pp-to-string scope-handler)
	  (pp-to-string config))
  (let ((fns (let ((app (if (listp config)
			    (car config)
			  config))
		   (config-options (if (listp config)
				       (cdr config)
				     nil)))
	       (collect-lists
		nil
		(append (cl-loop for phase in '(:pre-check :check :after-check)
				 collect (mk-scope-handler-call
					  scope-handler
					  :config
					  app
					  phase
					  config-options))
			(cl-loop for phase in '(:pre-prepare
						:prepare :after-prepare)
				 collect (mk-scope-handler-call
					  scope-handler
					  :prepare
					  app
					  phase
					  config-options))
			(list (make-config-hooks config))
			(list (mk-scope-handler-call
			       scope-handler
			       :activate
			       app
			       'ignore
			       config-options)))))))
    (DEBUG! "make-scope-help-fns fns %s"
	    (pp-to-string fns))
    fns))

;; define configuration scopes
;;
;; (mode +mode_feature -mode-feature)
(defun call-mode-features (mode action phase features)
  (DEBUG! "call-mode-features mode %s action %s phase %s features %s major mode %s"
	  mode action phase features major-mode)
  (cl-loop for f in (auto-features (format "%s-mode" mode))
	   do (let ((options (plist-put nil :mode mode)))
		(invoke-feature f action 'modes phase
				(plist-put options
					   :status 1))))
  (when features
    (let ((z (normalize-non-keyword-options features)))
      (DEBUG! "z = %s " z)
      (let ((z-features (filt-out-non-keywords (collect-keyword-values z))))
	(DEBUG! "z-features = %s" z-features)
	(let ((options (plist-put nil :mode mode)))
	  (cl-loop for feature in z-features
		   do (progn
			(DEBUG! "feature %s options %s"
				feature options)
			(pcase feature
			  (:default t)
			  (_ (invoke-feature `,(intern (keyword-name
							feature))
					     action 'modes phase
					     (plist-put options
							:status
							(plist-get z
								   feature))))))))))))

(defun collect-mode-pkgs (mode features)
  (let ((auto-feature-pkgs
	 (collect-lists nil
			(cl-loop for f in (auto-features (format "%s-mode" mode))
				 collect (let ((options (plist-put nil :mode mode)))
					   (invoke-feature f 'pkglist
							   'modes 'ignore
							   options))))))
    (let ((pkgs1 (if (not features)
		     nil
		   (let ((z (normalize-non-keyword-options features)))
		     (let ((z-features (filt-out-non-keywords (collect-keyword-values z)))
			   (options (plist-put nil :mode mode)))
		       (collect-lists nil
				      (cl-loop for feature in z-features
					       collect (pcase feature
							 (:default nil)
							 (_ (invoke-feature `,(intern (keyword-name feature))
									    'pkglist 'modes
									    'ignore
									    (plist-put options
										       :status
										       (plist-get z feature))))))))))))
      (DEBUG! "collect-mode-pkgs %s %s"
	      auto-feature-pkgs
	      pkgs1)
      (append pkgs1
	      auto-feature-pkgs))))

(defun config-mode-features (mode phase features)
  (call-mode-features mode 'configure phase features))

(defun mode-feature-config (mode phase options)
  (DEBUG! "mode feature config mode %s phase %s options %s"
	  mode phase options)
  (let ((config-options (collect-keyword-values options)))
    (let ((features (plist-get config-options :features)))
      (DEBUG! "mode %s config-options %s features %s"
	      mode config-options features)
      (pcase phase
	(:pre-check
	 (progn
	   (let ((suffixes (plist-get config-options :suffix)))
	     (cl-loop for s in suffixes
		      do (assoc-suffix-to-mode s mode)))))
	(_ t))
      (config-mode-features mode phase features))))

(defun mode-feature-prepare (mode phase options)
  (DEBUG! "mode feature prepare mode %s phase %s options %s"
	  mode phase options)
  (let ((config-options (collect-keyword-values options)))
    (let ((features (plist-get config-options :features)))
      (call-mode-features mode 'prepare phase features))))

(defun mode-feature-activate (mode phase options)
  (DEBUG! "mode feature activate mode %s phase %s options %s"
	  mode phase options)
  (let ((config-options (collect-keyword-values options)))
    (let ((features (plist-get config-options :features)))
      (DEBUG! "mode-feature-activate mode %s features %s config-options %s"
	      mode features config-options)
      (fset (intern (format "activate-features/:%s-mode" mode))
	    `(lambda ()
	       (call-mode-features ',mode 'activate
				   ',phase ',features))))))

(defun mode-feature-pkgs (mode options)
  (let ((config-options (collect-keyword-values options)))
    (let ((features (plist-get config-options :features)))
      (collect-mode-pkgs mode features))))

;; (ui_feature options ...)
;; app
;; (app +options -options)
(defun app-feature-config (app scope phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "config app %s scope %s phase %s options %s"
	    app scope phase options)
    (invoke-feature app 'configure scope
		    phase options)))

(defun app-feature-prepare (app scope phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "prepare app %s scope %s phase %s options %s"
	    app scope phase options)
    (invoke-feature app 'prepare scope
		    phase options)))

(defun app-feature-activate (app scope phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "activate app %s scope %s options %s"
	    app scope options)

    (after-boot-run #'(lambda (app options)
			(invoke-feature app 'activate scope
					phase options))
		    app options)))

(defun make-scope-by-config (key config)
  (pcase key
    (:init
     `(,@(config/:make-scope 'init (list config))))
    ((or :core :modes :ui :completion :app :editor)
     (let ((scope-class (intern (keyword-name key))))
       `(,@(config/:make-scope scope-class config))))
    (_ nil)))


(defvar scope-priorities
  '(:init 0 :core 1 :editor 10 :modes 15  :completion 50 :app 100)
  "priority/order of scope")

(defvar scope-default-priority 500
  "The default priority for scopes which have not been listed in scope-priorities")

(defun scope-priority (scope)
  (let ((prio (plist-get scope-priorities scope)))
    (if prio
	prio
      scope-default-priority)))

(defun scope<= (scope1 scope2)
  (<= (scope-priority scope1)
      (scope-priority scope2)))

(defun sort-scopes (scopes)
  (sort scopes 'scope<=))

(provide 'core-scope)
