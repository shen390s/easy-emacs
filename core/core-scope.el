;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'core-lib)
(require 'core-log)
(require 'core-modules)

(defclass Base-Config ()
  ((name :initarg :name
	 :initform "default")
   (config :initarg :config
	   :initform nil)
   (pre-check :initarg :pre-check
	      :initform nil)
   (check :initarg :check
	  :initform nil)
   (after-check :initarg :after-check
		:initform nil)
   (pre-activate :initarg :pre-activate
		 :initform nil)
   (activate :initarg :activate
	     :initform nil)
   (after-activate :initarg :after-activate
		   :initform nil)
   (pkgs :initarg :pkgs
	 :initform nil)
   (pkg-installed :initarg :pkg-installed
		  :initform nil))
  "Configuration in scope")

(defmethod Config/Check:before ((config Base-Config) scope-name)
  (with-slots (pre-check name) config
    (when pre-check
      (funcall pre-check scope-name name))))

(defmethod Config/Check:primary ((config Base-Config) scope-name)
  (with-slots (check name) config
    (when check
      (funcall check scope-name name))))

(defmethod Config/Check:after ((config Base-Config) scope-name)
  (with-slots (after-check name) config
    (when after-check
      (funcall after-check scope-name name))))

(defmethod Config/Activate:before ((config Base-Config) scope-name)
  (with-slots (pre-activate name) config
    (when pre-activate
      (funcall pre-activate scope-name name))))

(defmethod Config/Activate:primary ((config Base-Config) scope-name)
  (with-slots (activate name) config
    (when activate
      (funcall activate scope-name name))))

(defmethod Config/Activate:after ((config Base-Config) scope-name)
  (with-slots (after-activate name) config
    (when after-activate
      (funcall after-activate scope-name name))))

(defmethod Config/GetPkgs ((config Base-Config))
  (oref config pkgs))

(defclass Vars-Config (Base-Config)
  ()
  "for configuration variables")

(defclass Mode-Config (Base-Config)
  ((enabled-features :initarg :actived-features
		     :initform nil)
   (disabled-features :initarg :disabled-features
		      :initform nil)
   (activate-feature :initarg :activate-feature
		     :initform nil)
   (config-feature :initarg :config-feature
		   :initform nil)
   (enable-feature :initarg :enable-feature
		   :initform nil)
   (disable-feature :initarg :disable-feature
		    :initform nil)
   (deactivate-feature :initarg :deactivate-feature
		       :initform nil))
  "Configuration for modes")

(defclass UI-Config (Base-Config)
  ((theme :initarg :theme
	  :initform nil))
  "UI Related configurations")

(defclass Completion-Config (Base-Config)
  ((completion :initarg :completion
	       :initform nil))
  "Configuration of completion")

(defclass App-Config (Base-Config)
  ((apps :initarg :apps
	 :initform nil))
  "Application Configuration")

(defclass Scope ()
  ((name :initarg :name
	 :initform "null-scope")
   (configs :initarg :configs
	    :initform nil))
  "Class scope for EasyEmacs")

(defun scope-hook-name (scope-name hook-type)
  (intern (format "scope-%s-%s-hooks"
		  scope-name
		  hook-type)))

(defmethod Object/to-string ((scope Scope))
  (format "Scope name:%s  enabled features: %s disabled features %s"
	  (oref scope name)
	  (oref scope enabled-features)
	  (oref scope disabled-features)))

(defmacro with-scope! (scope-name scope-var &rest body)
  `(when ,scope-name
     (let ((,scope-var (get-scope ,scope-name)))
       (when ,scope-var
	 ,@body))))

(defmethod Scope/name ((scope Scope))
  (oref scope name))

(defmethod Scope/add-config ((scope Scope) config)
  (push config (oref scope configs)))

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
  (let ((actions '(Configure Activate))
	(phases '(before primary after)))
    `(progn
       ,@(collect-lists nil
			(cl-loop for action in actions
				 collect (cl-loop for phase in phases
						  collect (make-scope-method action
									     phase)))))))
(make-scope-methods!)

(defmethod Scope/install-pkgs ((scope Scope))
  (let ((pkg-installed (oref scope pkg-installed)))
    (unless pkg-installed
      (cl-loop for pkg in
	       (packages (mapcar #'(lambda (f)
				     (plist-get f :name))
				 (oref scope enabled-features)))
	       do (install-package-by-name pkg))
      (setf (oref scope pkg-installed) t))))

(defvar all-scopes (make-hash-table)
  "All defined scopes")

(defun make-scope (scope-name &optional configs)
  (DEBUG! "make-scope %s %s"
	  scope-name configs)
  (let ((scope (make-instance 'Scope
			      :name scope-name
			      :configs nil)))
    (cl-loop for c in configs
	     do (Scope/add-config scope c))
    (puthash scope-name scope all-scopes)))

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

(defun make-config (config name fns)
  (DEBUG! "make-config %s %s %s"
	  config name fns)
  (make-instance config
		 :name name
		 :pre-check (plist-get fns :pre-check)
		 :check (plist-get fns :check)
		 :after-check (plist-get fns :after-check)
		 :pre-activate (plist-get fns :pre-activate)
		 :activate (plist-get fns :activate)
		 :after-activate (plist-get fns :after-activate)))

(defun config/:name (scope config)
  (pcase scope
    ('vars 'vars)
    (_ (if (listp config)
	   (if config
	       (car config)
	     'Null)
	 `,config))))

(defun config/:make-config (scope config)
  (pcase scope
    ('vars
     (make-config 'Vars-Config
		  (config/:name scope config)
		  (make-vars-help-fns config)))
    ('modes
     (make-config 'Mode-Config
		  (config/:name scope config)
		  (make-mode-help-fns config)))
    ('ui
     (make-config 'UI-Config
		  (config/:name scope config)
		  (make-ui-help-fns config)))
    ('completion
     (make-config 'Completion-Config
		  (config/:name scope config)
		  (make-completion-help-fns config)))
    ('app
     (make-config 'App-Config
		  (config/:name scope config)
		  (make-app-help-fns config)))
    (_
     nil)))

(defun config/:make-scope (scope configs)
  `((DEBUG! "config/:make-%s %s"
	    ',scope ',configs)
    (scope! ,(intern (symbol-name scope)))
    ,@(cl-loop for config in configs
	       collect `(let ((c (config/:make-config ',scope
						      ',config)))
			  (when c
			    (setf (oref c config) ',config)
			    (with-scope! ',(intern (symbol-name scope))
					 scope
					 (Scope/add-config scope c)))))))
;; define configuration scopes
;; (vars (a . 1) (b . 2) ...)
(defun make-vars-help-fns (config)
  (list :pre-check
	`(lambda (scope-name config-name)
	   ,@(cl-loop for var in config
		      collect `(setq ,(car var)
				     ,(cdr var))))))

(defun config/:make-vars (config)
  (config/:make-scope 'vars (list config)))
      
;; (mode +mode_feature -mode-feature)
(defun make-mode-help-fns (config)
  (let ((mode-name (car config))
	(mode-config (collect-keyword-values (cdr config))))
    (let ((features (plist-get mode-config :features))
	  (suffixes (plist-get mode-config :suffix)))
      (list :pre-check
	    `(lambda (scope-name config-name)
	       ,@(cl-loop for s in suffixes
			  collect (gen-add-suffix-to-mode s mode-name)))))))


(defun config/:make-modes (configs)
  (config/:make-scope 'modes configs))

;; (theme ...)
(defun make-ui-help-fns (config)
  t)

(defun config/:make-ui (configs)
  (config/:make-scope 'ui configs))

;; (+ivy -autocompletion )
(defun make-completion-help-fns (config)
  nil)

(defun config/:make-completion (config)
  (config/:make-scope 'completion (list config)))

;; app
;; (app +options -options)
(defun make-app-help-fns (config)
  (DEBUG! "make-app-help-fns %s" config)
  nil)

(defun config/:make-app (configs)
  (config/:make-scope 'app configs))

(defun make-scope-by-config (key config)
  (pcase key
      (:vars
       `(,@(config/:make-vars config)))
      (:modes
       `(,@(config/:make-modes config)))
      (:ui
       `(,@(config/:make-ui config)))
      (:completion
       `(,@(config/:make-completion config)))
      (:app
       `(,@(config/:make-app config)))
    (_ nil)))

;; Just for loading
(defmacro add-scope-hook (&rest args)
  t)

(defun actived-features ()
  t)

(defun pkgs-needed ()
  (let ((pkgs nil))
    (foreach-scope! scope-name scope
		    (setq pkgs
			  (append pkgs
				  (Scope/GetPkgs scope))))
    (delete-dups pkgs)))

(provide 'core-scope)
