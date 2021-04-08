;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'core-lib)
(require 'core-log)
(require 'core-modules)

(defclass Scope-Config ()
  ((name :initarg :name
	 :initform "default")
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

(defmethod Config/Check:before ((config Scope-Config) scope-name)
  (with-slots (pre-check) config
    (when pre-check
      (funcall pre-check scope-name))))

(defmethod Config/Check:primary ((config Scope-Config) scope-name)
  (with-slots (check) config
    (when check
      (funcall check scope-name))))

(defmethod Config/Check:after ((config Scope-Config) scope-name)
  (with-slots (after-check) config
    (when after-check
      (funcall after-check scope-name))))

(defmethod Config/Activate:before ((config Scope-Config) scope-name)
  (with-slots (pre-activate) config
    (when pre-activate
      (funcall pre-activate scope-name))))

(defmethod Config/Activate:primary ((config Scope-Config) scope-name)
  (with-slots (activate) config
    (when activate
      (funcall activate scope-name))))

(defmethod Config/Activate:after ((config Scope-Config) scope-name)
  (with-slots (after-activate) config
    (when after-activate
      (funcall after-activate scope-name))))

(defclass Mode-Config (Scope-Config)
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

(defclass UI-Config (Scope-Config)
  ((theme :initarg :theme
	  :initform nil))
  "UI Related configurations")

(defclass Completion-Config (Scope-Config)
  ((completion :initarg :completion
	       :initform nil))
  "Configuration of completion")

(defclass App-Config (Scope-Config)
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

;; define configuration scopes
;; (vars (a . 1) (b . 2) ...)
(defun config/:make-vars (config)
  `((DEBUG! "config/:make-vars %s" ',config)
    (scope! vars)
    (let ((c (make-instance 'Scope-Config
			    :pre-activate
			    '(lambda (scope-name)
			       ,@(cl-loop for var in config
					  collect `(setq ,(car var)
							 ,(cdr var)))))))
      (with-scope! 'vars scope
		   (Scope/add-config scope c)))))
      
;; (mode +mode_feature -mode-feature)
(defun config/:make-modes (config)
  `((DEBUG! "config/:make-modes %s" ',config)))

;; (theme ...)
(defun config/:make-ui (config)
  `((DEBUG! "config/:make-ui %s" ',config)))
;; (+ivy -autocompletion )

(defun config/:make-completion (config)
  `((DEBUG! "config/:make-completion %s" ',config)))
;; (app1 app2 ...)
(defun config/:make-app (config)
  `((DEBUG! "config/:make-app %s" ',config)))


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
    (- nil)))

;; Just for loading
(defmacro add-scope-hook (&rest args)
  t)

(defun actived-features ()
  nil)

(provide 'core-scope)
