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
   (pkg-installed :initarg :pkg-installed
		  :initform nil))
  "Configuration in scope")

(defmethod Config/Check :before ((config Scope-Config) scope-name)
  (with-slots (pre-check) config
    (when pre-check
      (funcall pre-check scope-name))))

(defmethod Config/Check :primary ((config Scope-Config) scope-name)
  (with-slots (check) config
    (when check
      (funcall check scope-name))))

(defmethod Config/Check :after ((config Scope-Config) scope-name)
  (with-slots (after-check) config
    (when after-check
      (funcall after-check scope-name))))

(defmethod Config/Activate :before ((config Scope-Config) scope-name)
  (with-slots (pre-activate) config
    (when pre-activate
      (funcall pre-activate scope-name))))

(defmethod Config/Activate :primary ((config Scope-Config) scope-name)
  (with-slots (activate) config
    (when activate
      (funcall activate scope-name))))

(defmethod Config/Activate :after ((config Scope-Config) scope-name)
  (with-slots (after-activate) config
    (when after-activate
      (funcall after-activate scope-name))))

(defmacro setvars! (vars)
  `(progn
     ,@(cl-loop  for var in vars
		 collect `(setq ,(car var) ,(cdr var)))))

(defmethod Config/Activate ((config Scope-Config) scope-name)
  (with-slots (vars) config
    `(setvars! ,vars)))

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
   (parse-config :initarg :parse-config
		 :initform nil)
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

(defmethod Scope/parse-configs ((scope Scope)  configs)
  (with-slots (parse-config) scope
    (when parse-config
      (funcall parse-config scope configs))))

(defmethod Scope/Configure ((scope Scope))
  (DEBUG! "Configure scope %s" scope)
  (let ((scope-name (oref scope name)))
    (with-slots (configs) scope
      (cl-loop for config in configs
	       do (Config/Check config scope-name))))
  t)

(defmethod Scope/Activate :before ((scope Scope))
  (DEBUG! "Activate scope/:before %s" scope)
  t)

(defmethod Scope/Activate :after ((scope Scope))
  (DEBUG! "Activate scope/:after %s" scope)
  t)

(defmethod Scope/Activate :primary ((scope Scope))
  (DEBUG! "Activate scope/:primary %s" scope)
  t)

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

(defun make-scope (scope-name &optional parse-config)
  (DEBUG! "make-scope %s %s"
	  scope-name parse-config)
  (let ((scope (make-instance 'Scope
			      :name scope-name
			      :configs nil
			      :parse-config parse-config)))
      (puthash scope-name scope all-scopes)))

(defmacro scope! (name &optional parse-config) 
  `(progn
     (make-scope ',name ,parse-config)))

(defun get-scope (name)
  (let ((scope (gethash name all-scopes)))
    (DEBUG2! "get-scope %s"
	     (Object/to-string scope))
    scope))

(defun install-packages-for-scope (scope-name)
  (with-scope! scope-name
	      scope
	      (Scope/install-pkgs scope)))

(defun make-scope-by-config (key config)
  (pcase key
      (:vars
       `((DEBUG! "make vars by config %s" ',config)))
      (:modes
       `((DEBUG! "make mode scope config %s" ',config)))
      (:ui
       `((DEBUG! "make ui scope by config %s" ',config)))
      (:completion
       `((DEBUG! "make completion scope by config %s" ',config)))
      (:app
       `((DEBUG! "make app scope by configuration %s" ',config)))
    (- nil)))

;; define configuration scopes
;; (vars (a . 1) (b . 2) ...)
(defun do-vars/:parse-config (config)
  nil)

(defun vars/:parse-config (scope configs)
  (DEBUG! "config/:parse-config scope %s configs %s"
	  scope configs)
  (cl-loop for config in configs
	   do (let ((c (do-vars/:parse-config config)))
		(when c
		  (DEBUG! "c = %s" c)
		  (Scope/add-config scope c))))
  t)

;; (mode +mode_feature -mode-feature)
(defun modes/:parse-config (scope configs)
  (DEBUG! "modes/:parse-config scope %s configs %s"
	  scope configs)
  t)

;; (theme ...)
(defun ui/:parse-config (scope configs)
  (DEBUG! "ui/:parse-config scope %s configs %s"
	  scope configs)
  t)

;; (+ivy -autocompletion )
(defun completion/:parse-config (scope configs)
  (DEBUG! "completion/:parse-config scope %s configs %s"
	  scope configs)
  t)

;; (app1 app2 ...)
(defun app/:parse-config (scope configs)
  (DEBUG! "app/:parse-config scope %s configs %s"
	  scope configs)
  t)

;; Just for loading
(defmacro add-scope-hook (&rest args)
  t)

(defun actived-features ()
  nil)

(provide 'core-scope)
