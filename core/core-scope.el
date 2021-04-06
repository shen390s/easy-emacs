;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'core-lib)
(require 'core-log)
(require 'core-modules)

(defclass Scope ()
  ((name :initarg :name
	 :initform "null-scope")
   (features :initarg :features
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
  "Class scope for EasyEmacs")

(defun scope-hook-name (scope-name hook-type)
  (intern (format "scope-%s-%s-hooks"
		  scope-name
		  hook-type)))

(defmacro with-scope! (scope-name scope-var &rest body)
  `(when ,scope-name
     (let ((,scope-var (get-scope ,scope-name)))
       (when ,scope-var
	 ,@body))))

(defmethod Scope/name ((scope Scope))
  (oref scope name))

(defmethod Scope/add-feature ((scope Scope) feature)
  (progn
    (push feature (oref scope features))))

(defmethod Scope/install-pkgs ((scope Scope))
  (let ((pkg-installed (oref scope pkg-installed)))
    (unless pkg-installed
      (cl-loop for pkg in
	       (packages (mapcar #'(lambda (f)
				     (plist-get f :name))
				 (oref scope features)))
	       do (install-package-by-name pkg))
      (setf (oref scope pkg-installed) t))))

(defvar all-scopes (make-hash-table)
  "All defined scopes")

(defvar current-scope nil
  "The current scope which will be activated")

(defun make-scope (scope-name &optional activate-fn
			      config-fn enable-fn
			      disable-fn deactivate-fn)
  (let ((scope (make-instance 'Scope
			      :name scope-name
			      :features nil
			      :activate-feature activate-fn
			      :config-feature config-fn
			      :enable-feature enable-fn
			      :disable-feature disable-fn
			      :deactivate-feature deactivate-fn)))
      (puthash scope-name scope all-scopes)))

(defvar scope-keywords nil
  "All list of supported keywords for configuration")

(defmacro scope! (name &optional activate-fn
		       config-fn enable-fn
		       disable-fn deactivate-fn)
  `(progn
     (make-scope ',name )
     (push (intern (format ":%s" ',name))
	   scope-keywords)))

(defun get-scope (name)
  (let ((scope (gethash name all-scopes)))
    (DEBUG2! "get-scope %s"
	     (Object/to-string scope))
    scope))

(defun install-packages-for-scope (scope-name)
  (with-scope! scope-name
	      scope
	      (Scope/install-pkgs scope)))

(provide 'core-scope)
