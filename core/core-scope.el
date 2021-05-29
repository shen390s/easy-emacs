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

(defmethod Config/Call ((config Base-Config) fn scope)
  (with-slots (fns) config
    (let ((f (plist-get fns fn)))
      (when f
	(funcall f)))))

(defmethod Config/make-init ((config Base-Config))
  (with-slots (name fns) config
    (cl-loop for hook in '(:before-activate :after-activate)
	     do (let ((fn (plist-get fns hook)))
		  (when fn
		    (add-hook (intern (format "%s-%s-hook"
					      name
					      (keyword-name hook)))
			      `,@fn))))))

(defun make-config-method (key name)
  (DEBUG! "make-config-method key %s name %s" (pp-to-string key) name)
  `(defmethod ,(intern (format "Config/%s" name))
     ((config Base-Config) scope)
     (Config/Call config ,key scope)))

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

(defgeneric Config/Pkgs:update ((config Base-Config) scope-name))

(defmethod Config/Pkgs:get ((config Base-Config))
  (oref config pkgs))


(defclass InitialSettings-Config (Base-Config)
  ()
  "for initial settings")

(defmethod Config/make-init ((config InitialSettings-Config))
  (with-slots (config) config
    (setf fns (list :pre-prepare
		    `(lambda ()
		       ,@config))))
  (call-next-method))

(defmethod Config/Pkgs:update ((config InitialSettings-Config) scope-name)
  nil)

(defclass Core-Config (Base-Config)
  ()
  "Configuration for core")

(defmethod Config/make-init ((config Core-Config))
  (with-slots (fns config) config
    (setf fns (make-scope-help-fns (list :config #'core-feature-config
					 :prepare #'core-feature-prepare
					 :activate #'core-feature-activate)
				   config)))
  (call-next-method))

(defmethod Config/Pkgs:update ((config Core-Config) scope)
  nil)

(defclass Mode-Config (Base-Config)
  ()
  "Configuration for modes")

(defmethod Config/make-init ((config Mode-Config))
  (with-slots (fns config) config
    (setf fns (make-scope-help-fns (list :config #'mode-feature-config
					 :prepare #'mode-feature-prepare
					 :activate #'mode-feature-activate)
				   config)))
  (call-next-method))

(defmethod Config/Pkgs:update ((config Mode-Config) scope-name)
  nil)

(defclass UI-Config (Base-Config)
  ((theme :initarg :theme
	  :initform nil))
  "UI Related configurations")

(defmethod Config/make-init ((config UI-Config))
  (with-slots (fns config) config
    (setf fns (make-scope-help-fns (list :config #'ui-config
					 :prepare #'ui-prepare
					 :activate #'ui-activate)
				   config)))
  (call-next-method))

(defmethod Config/Pkgs:update ((config UI-Config) scope-name)
  nil)

(defclass Completion-Config (Base-Config)
  ((completion :initarg :completion
	       :initform nil))
  "Configuration of completion")

(defmethod Config/make-init ((config Completion-Config))
  (with-slots (fns config) config
    (setf fns (make-scope-help-fns (list :config #'completion-config
					 :prepare #'completion-prepare
					 :activate #'completion-activate)
				   config)))
  (call-next-method))

(defmethod Config/Pkgs:update ((config Completion-Config) scope-name)
  nil)

(defclass App-Config (Base-Config)
  ((apps :initarg :apps
	 :initform nil))
  "Application Configuration")

(defmethod Config/make-init ((config App-Config))
  (with-slots (fns config) config
    (setf fns (make-scope-help-fns (list :config #'app-feature-config
					 :prepare #'app-feature-prepare
					 :activate #'app-feature-activate)
				   config)))
  (call-next-method))

(defmethod Config/Pkgs:update ((config App-Config) scope-name)
  (DEBUG! "update package requires for %s scope %s"
	  config scope-name)
  (with-slots (config) config
    (let ((app (if (listp config)
		   (car config)
		 config))
	  (options (if (listp config)
		       (cdr config)
		     nil)))
      (DEBUG! "app = %s options = %s" app options)
      (invoke-feature app 'pkglist 'app 'ignore options))))

(defclass Editor-Config (Base-Config)
  ()
  "Editor Configuration")

(defmethod Config/make-init ((config Editor-Config))
  (with-slots (fns config) config
    (setf fns (make-scope-help-fns (list :config #'editor-feature-config
					 :prepare #'editor-feature-prepare
					 :activate #'editor-feature-activate)
				   config)))
  (call-next-method))

(defmethod Config/Pkgs:update ((config Editor-Config) scope)
  t)

(defclass Scope ()
  ((name :initarg :name
	 :initform "null-scope")
   (configs :initarg :configs
	    :initform nil))
  "Class scope for EasyEmacs")

(defmethod Object/to-string ((scope Scope))
  (format "scope %s" scope))

(defmacro with-scope! (scope-name scope-var &rest body)
  `(when ,scope-name
     (let ((,scope-var (get-scope ,scope-name)))
       (when ,scope-var
	 ,@body))))

(defmethod Scope/name ((scope Scope))
  (with-slots (name) scope
    name))

(defmethod Scope/add-config ((scope Scope) config)
  (with-slots (configs) scope
    (push config configs)))

(defmethod Scope/get-pkgs ((scope Scope))
  (collect-lists nil
		 (with-slots (configs name) scope
		   (cl-loop for config in configs
			    collect (progn
				      (Config/Pkgs:update config name)
				      (Config/Pkgs:get config))))))

(defun make-scope-method (action phase)
  `(defmethod ,(intern (format "Scope/%s:%s" action phase))
     ((scope Scope))
     (with-slots (name configs) scope
       (cl-loop  for config in configs
		 do (,(intern (format "Config/%s:%s"
				      (if (string= action "Configure")
					  "Check"
					action)
				      phase))
		     config name)))))

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

(defmethod Scope/Activate ((scope Scope))
  (DEBUG! "activate scope %s"
	  (pp-to-string scope))
  (with-slots (name configs) scope
    (cl-loop for config in configs
	     do (Config/Activate config name))))

(defmethod Scope/install-pkgs ((scope Scope))
  (let ((pkg-installed (oref scope pkg-installed)))
    (unless pkg-installed
      (cl-loop for pkg in (Scope/get-pkgs scope)
	       do (install-package-by-name pkg))
      (setf (oref scope pkg-installed) t))))

(defmethod Scope/make-config ((scope Scope) config)
  (let ((config-name (Scope/config-name scope config))
	(config-class (Scope/config-class scope)))
    (when config-class
      (make-config config-class
		   config-name
		   config))))

(defmethod Scope/config-name ((scope Scope) config)
  (if (listp config)
      (if config
	  (car config)
	'Null)
    `,config))

(defmethod Scope/config-class ((scope Scope))
  nil)

(defclass Initial-Scope (Scope)
  ()
  "Scope for initialization")

(defmethod Scope/config-name ((scope Initial-Scope) config)
  'init)

(defmethod Scope/config-class ((scope Initial-Scope))
  'InitialSettings-Config)

(defclass Core-Scope (Scope)
  ()
  "Core scope of easy-emacs")

(defmethod Scope/config-class ((scope Core-Scope))
  'Core-Config)

(defclass Mode-Scope (Scope)
  ()
  "Scope for modes")

(defmethod Scope/config-class ((scope Mode-Scope))
  'Mode-Config)

(defclass UI-Scope (Scope)
  ()
  "Scope for UI")

(defmethod Scope/config-class ((scope UI-Scope))
  'UI-Config)

(defclass Completion-Scope (Scope)
  ()
  "Scope for completion")

(defmethod Scope/config-class ((scope Completion-Scope))
  'Completion-Config)

(defclass App-Scope (Scope)
  ()
  "Scope for Application")

(defmethod Scope/config-class ((scope App-Scope))
  'App-Config)

(defclass Editor-Scope (Scope)
  ()
  "Scope for editor related")

(defmethod Scope/config-class ((scope Editor-Scope))
  'Editor-Config)

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

(defun install-packages-for-scope (scope-name)
  (with-scope! scope-name
	       scope
	       (Scope/install-pkgs scope)))

(defmacro foreach-scope! (name scope &rest body)
  `(maphash '(lambda (,name ,scope)
	       ,@body)
	    all-scopes))

(defun make-config (config name configs)
  (DEBUG! "make-config %s %s %s"
	  config name (pp-to-string configs))
  (let ((c (make-instance config
			  :name name
			  :config configs)))
    (Config/make-init c)
    c))

(defun config/:make-scope (scope-name configs)
  `((DEBUG! "config/:make-%s %s"
	    ',scope-name ',(pp-to-string configs))
    (scope! ,(intern (symbol-name scope-name)))
    (with-scope! ',(intern (symbol-name scope-name)) scope
		 ,@(cl-loop for config in configs
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
(defun core-feature-config (core phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "config core %s phase %s options %s"
	    core phase options)
    (invoke-feature core 'configure 'core
		    phase options)))

(defun core-feature-prepare (core phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "prepare core %s phase %s options %s"
	    core phase options)
    (invoke-feature core 'prepare 'core
		    phase options)))

(defun core-feature-activate (core phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "activate core %s options %s"
	    core options)

    (after-boot-run #'(lambda (core options)
			(invoke-feature core 'activate 'core
					'ignore options))
		    core options)))

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
      (add-hook `,(intern (format "%s-mode-hook" mode))
		`(lambda ()
		   (call-mode-features ',mode 'activate
				       ',phase ',features))))))

;; (ui_feature options ...)
(defun ui-config (ui phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "ui-config ui %s phase %s options %s"
	    ui phase options)
    (invoke-feature ui 'configure
		    'ui phase options)))

(defun ui-prepare (ui phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "ui-prepare ui %s phase %s options %s"
	    ui phase options)
    (invoke-feature ui 'prepare
		    'ui phase options)))

(defun ui-activate (ui phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "ui-activate ui %s phase %s options %s"
	    ui phase options)
    (after-boot-run #'(lambda (ui options)
			(invoke-feature ui 'activate 'ui
					'ignore options))
		    ui options)))

;; (+ivy -autocompletion )

(defun completion-config (app phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "completion configure app %s phase %s options %s"
	    app phase options)
    (invoke-feature app 'configure
		    'completion phase options)))

(defun completion-prepare (app phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "completion prepare app %s phase %s options %s"
	    app phase options)
    (invoke-feature app 'prepare
		    'completion phase options)))

(defun completion-activate (compl phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (after-boot-run #'(lambda (compl options)
			(invoke-feature compl 'activate 'completion
					'ignore options))
		    compl options)))

;; app
;; (app +options -options)
(defun app-feature-config (app phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "config app %s phase %s options %s"
	    app phase options)
    (invoke-feature app 'configure 'app
		    phase options)))

(defun app-feature-prepare (app phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "prepare app %s phase %s options %s"
	    app phase options)
    (invoke-feature app 'prepare 'app
		    phase options)))

(defun app-feature-activate (app phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "activate app %s options %s"
	    app options)

    (after-boot-run #'(lambda (app options)
			(invoke-feature app 'activate 'app
					'ignore options))
		    app options)))

(defun editor-feature-config (editor phase options)
  (let ((options (normalize-options options)))
    (DEBUG! "editor-feature-config editor %s phase %s options %s"
	    editor phase options)
    (invoke-feature editor 'configure 'editor
		    phase (plist-put options :status 1))))

(defun editor-feature-prepare (editor phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (DEBUG! "editor-feature-prepare editor %s phase %s options %s"
	    editor phase options)
    (invoke-feature editor 'prepare 'editor
		    phase options)))

(defun editor-feature-activate (editor phase options)
  (let ((options (plist-put (normalize-options options)
			    :status 1)))
    (after-boot-run #'(lambda (editor options)
			(invoke-feature editor 'activate 'editor
					'ignore options))
		    editor options)))

(defun make-scope-by-config (key config)
  (pcase key
    (:init
     `(,@(config/:make-scope 'init (list config))))
    ((or :core :modes :ui :completion :app :editor)
     (let ((scope-class (intern (keyword-name key))))
       `(,@(config/:make-scope scope-class config))))
    (_ nil)))


(defvar scope-priorities
  '(:init 0 :core 1 :editor 10 :modes 15 :editor 25 :completion 50 :app 100)
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
