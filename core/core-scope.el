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
   (enabled-features :initarg :actived-features
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
		       :initform nil)
   (pkg-installed :initarg :pkg-installed
		  :initform nil))
  "Configuration in scope")

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

(defmethod Scope/add-feature ((scope Scope) feature-name)
  (let ((feature (parse-feature feature-name)))
    (if (plist-get feature :disabled)
	(push feature (oref scope disabled-features))
      (push feature (oref scope enabled-features)))))

(defmethod Scope/configure-feature ((scope Scope) feature)
  t)

(defmethod Scope/enable-feature ((scope Scope) feature)
  t)

(defmethod Scope/disable-feature ((scope Scope) feature)
  t)

(defmethod Scope/deactivate-feature ((scope Scope) feature)
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

;; define configuration scopes
(defun config/:parse-config (scope configs)
  (DEBUG! "config/:parse-config scope %s configs %s"
	  scope configs)
  t)

(scope! config 'config/:parse-config)

(defun modes/:parse-config (scope configs)
  (DEBUG! "modes/:parse-config scope %s configs %s"
	  scope configs)
  t)

(scope! modes 'modes/:parse-config)

(defun ui/:parse-config (scope configs)
  (DEBUG! "ui/:parse-config scope %s configs %s"
	  scope configs)
  t)

(scope! ui 'ui/:parse-config)

(defun completion/:parse-config (scope configs)
  (DEBUG! "completion/:parse-config scope %s configs %s"
	  scope configs)
  t)

(scope! completion 'completion/:parse-config)

(defun app/:parse-config (scope configs)
  (DEBUG! "app/:parse-config scope %s configs %s"
	  scope configs)
  t)

(scope! app 'app/:parse-config)

;; Just for loading
(defmacro add-scope-hook (&rest args)
  t)

(defun actived-features ()
  nil)

(provide 'core-scope)
