;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'core-lib)
(require 'core-log)
(require 'core-package)

(defgeneric Object/to-string (obj)
  "A generic method to convert object to string")

(defclass Package ()
  ((name :initarg :name
	 :initform "Anonymous")
   (docstring :initarg :docstring
	      :initform "")
   (init :initarg :init
	 :initform nil)
   (config :initarg :config
	   :initform nil)
   (pkg-info :initarg :pkg-info
	     :initform nil)
   (installed :initarg :installed
	      :initform nil))
  "Class to describe the package of Emacs")

(defmethod Package/install ((pkg Package))
  (unless (oref pkg installed)
    (when (fboundp 'install-pkg)
      (install-pkg (oref pkg pkg-info)))
    (setf (oref pkg installed) t)))

(defmethod Object/to-string ((obj Package))
  (format "Package name: %s pkginfo: %s installed: %s"
	  (oref obj name)
	  (oref obj pkg-info)
	  (oref obj installed)))

(defclass Feature ()
  ((name :initarg :name
	 :initform "Anonymous")
   (docstring :initarg :docstring
	      :initform "Anonymous")
   (pkgs :initarg :pkgs
	 :initform nil)
   (config-fn :initarg :config-fn
	      :initform nil)
   (on-fn :initarg :on-fn
	  :initform nil)
   (off-fn :initarg :off-fn
	   :initform nil))
  "Class to describe the feature of Emacs")

(defmethod Feature/configure ((feature Feature))
  (let ((result t))
    (let ((config-fn (oref feature config-fn)))
      (when config-fn
	(condition-case err
	    (setf result (funcall config-fn))
	  (error (WARN! "configure feature %s error %s"
			(Object/to-string feature) (error-message-string err))
		 nil))))
    (DEBUG2! "configure feature %s return %s"
	     (Object/to-string feature) result)
    result))

(defmethod Feature/pkglist ((feature Feature))
  (let ((pkgs (oref feature pkgs)))
    (let ((zpkgs (if (functionp pkgs)
		     (funcall pkgs)
		   pkgs)))
      (if (listp zpkgs)
	  zpkgs
	(list zpkgs)))))

(defmethod Object/to-string ((obj Feature))
  (format "Feature name: %s pkgs: %s config-fn: %s on-fn:%s off-fn:%s"
	  (oref obj name)
	  (oref obj pkgs)
	  (oref obj config-fn)
	  (oref obj on-fn)
	  (oref obj off-fn)))

(defclass Mode (Feature)
  ((scope :initarg :scope
	  :initform nil))
  "Major mode")

(defmethod Mode/bind-scope ((mode Mode) scope)
  (setf (oref mode scope) scope))

(defmethod Mode/active-fn ((mode Mode))
  (lambda (&rest args)
    (progn
      (cl-loop for pkg in (Feature/pkglist mode)
	       do (Package/install (get-package pkg)))
      (apply (oref mode on-fn) args))))

(defmethod Object/to-string ((obj Mode))
  (format "Mode name: %s pkgs: %s on-fn: %s"
	  (oref obj name)
	  (oref obj pkgs)
	  (oref obj on-fn)))

(defclass Scope ()
  ((name :initarg :name
	 :initform "null-scope")
   (parent :initarg :parent
	   :initform nil)
   (features :initarg :features
	     :initform nil)
   (pkg-installed :initarg :pkg-installed
		  :initform nil))
  "Class scope for EasyEmacs")

(defun scope-hook-name (scope-name hook-type)
  (intern (format "scope-%s-%s-hooks"
		  scope-name
		  hook-type)))

(defmacro with-scope (scope-name scope-var &rest body)
  `(when ,scope-name
     (let ((,scope-var (get-scope ,scope-name)))
       (when ,scope-var
	 ,@body))))

(defmethod Scope/add-feature ((scope Scope) feature)
  (progn
    (push feature (oref scope features))))


(defmethod Scope/parent ((scope Scope))
  (let ((parent-name (oref scope parent)))
    (if parent-name
	(get-scope parent-name)
      nil)))

(defmethod Scope/install-pkgs ((scope Scope))
  (let ((pkg-installed (oref scope pkg-installed)))
    (unless pkg-installed
      (with-scope (oref scope parent)
		  parent-scope
		  (Scope/install-pkgs parent-scope))
      (cl-loop for pkg in
	       (packages (mapcar #'(lambda (f)
				     (plist-get f :name))
				 (oref scope features)))
	       do (install-package-by-name pkg))
      (setf (oref scope pkg-installed) t))))

(defmethod Scope/pre-activate ((scope Scope))
  (DEBUG! "preactive scope %s"
	  (oref scope name))
  (progn
    (with-scope (oref scope parent)
		parent-scope
		(Scope/pre-activate parent-scope))
    (run-hooks (scope-hook-name (oref scope name)
				'before-activate))))

(defmethod Scope/post-activate ((scope Scope))
  (DEBUG! "post-activate scope %s"
	  (oref scope name))
  (progn
    (run-hooks (scope-hook-name (oref scope name)
				'after-activate))
    (with-scope (oref scope parent)
		parent-scope
		(Scope/post-activate parent-scope))))

(defmacro call-scope-fun-by-name (scope-name func &rest args)
  `(let ((fn (intern (format "%s-scope-%s"
			     ,scope-name
			     ,func))))
     (when (fboundp fn)
       (funcall fn ,@args))))


(defmethod Scope/enable-features ((scope Scope))
  (call-scope-fun-by-name (oref scope name)
			  'enable-features))

(defmethod Scope/disable-features ((scope Scope))
  (call-scope-fun-by-name (oref scope name)
			  'disable-features))

(defmethod Scope/deactivate-features ((scope Scope))
  (call-scope-fun-by-name (oref scope name)
			  'deactivate-features))

(defmethod Scope/activate-1 ((scope Scope))
  (with-scope (oref scope parent)
	      parent-scope
	      (Scope/activate-1 parent-scope))
  (Scope/enable-features scope))

(defmethod Scope/activate-2 ((scope Scope))
  (Scope/disable-features scope)
  (with-scope (oref scope parent)
	      parent-scope
	      (Scope/activate-2 parent-scope)))

(defmethod Scope/activate ((scope Scope))
  (Scope/activate-1 scope)
  (Scope/activate-2 scope))

(defmethod Scope/deactivate ((scope Scope))
  (Scope/deactivate-features scope)
  (with-scope (oref scope parent)
	      parent-scope
	      (Scope/deactivate parent-scope)))

(defmethod Scope/add-hook ((scope Scope) hook-type hook-func)
  (pcase hook-type
    ('before
     (add-hook (scope-hook-name (oref scope name)
				'before-activate)
	       hook-func))
    ('after
     (add-hook (scope-hook-name (oref scope name)
				'after-activate)
	       hook-func))
    (_ (progn
	 (ERR! "Unsupported hook type: %s" hook-type)))))

(defun add-scope-hook (scope-name hook-type hook-fn)
  (with-scope scope-name
	      scope
	      (Scope/add-hook scope
			      hook-type
			      hook-fn)))

(defmethod Object/to-string ((scope Scope))
  (format "Scope name:%s parent: %s features: %s"
	  (oref scope name)
	  (oref scope parent)
	  (oref scope features)))

(defvar all-packages (make-hash-table)
  "All defined packages")

(defvar all-features (make-hash-table)
  "All defined features")

(defvar all-modes (make-hash-table)
  "All defined models")

(defvar all-scopes (make-hash-table)
  "All defined scopes")

(defvar current-scope nil
  "The current scope which will be activated")

(defmacro package! (&rest args)
  `(let ((name (plist-get ',args :name))
	 (docstring (plist-get ',args :docstring))
	 (pkginfo (plist-get ',args :pkginfo))
	 (init-fn (lambda ()
		    ,(plist-get args :init)))
	 (config-fn (lambda ()
		      ,(plist-get args :config))))
     (let ((package (make-instance 'Package
				   :name name
				   :docstring docstring
				   :pkg-info pkginfo
				   :init init-fn
				   :config config-fn
				   :installed nil)))
       (progn
	 (puthash name package all-packages)))))

(defmacro feature! (name docstring pkgs config-fn on-fn off-fn)
  `(let ((feature (make-instance 'Feature
				 :name ',name
				 :docstring ,docstring
				 :pkgs ',pkgs
				 :config-fn ',config-fn
				 :on-fn ',on-fn
				 :off-fn ',off-fn)))
     (progn
       (puthash ',name feature all-features)
       (set (intern (concat (symbol-name ',name)
			    "-actived"))
	    nil))))

(defmacro mode! (name docstring pkgs config-fn active-fn)
  `(progn
     (let ((zmode (make-instance 'Mode
				 :name ',name
				 :docstring ,docstring
				 :pkgs ',pkgs
				 :config-fn ',config-fn
				 :scope nil
				 :on-fn ',active-fn
				 :off-fn nil)))
       (puthash ',name zmode all-modes)
       ;; mode is also a instance of features
       (puthash ',name zmode all-features))
     
     (defun ,name (&rest args)
       (interactive)
       (DEBUG! "activate mode %s"
	       ',name)
       (let ((mode (get-mode ',name)))
	 (apply (Mode/active-fn mode) args)))))

(defmacro scope! (name &optional parent)
  `(progn
     (let ((scope (make-instance 'Scope
				 :name ',name
				 :parent ',parent
				 :features nil)))
       (puthash ',name scope all-scopes)
       (defvar ,(scope-hook-name name 'before-activate) nil)
       (defvar ,(scope-hook-name name 'after-activate) nil))))

(defun install-package-by-name (pkg)
  (DEBUG! "installing package %s..." pkg)
  (let ((package (gethash pkg all-packages)))
    (when package
      (Package/install package))))

(defun get-package (pkg)
  (let ((package (gethash pkg all-packages)))
    (unless package
      (setq package (Package :name pkg
			     :pkg-info pkg
			     :installed nil))
      (puthash pkg package all-packages))
    (DEBUG2! "get-package %s"
	     (Object/to-string package))
    package))

(defun get-feature (f)
  (let ((feature (gethash f all-features)))
    (if feature
	(DEBUG2! "get-feature : %s"
		 (Object/to-string feature))
      (DEBUG! "get-feature %s not found"
	      f ))
    feature))

(defun get-mode (m)
  (let ((mode (gethash m all-modes)))
    (DEBUG2! "get-mode %s"
	     (Object/to-string mode))
    mode))

(defun get-mode-active-fn (m)
  (let ((mode (get-mode m)))
    (if mode
	(Mode/active-fn mode)
      (intern (symbol-name m)))))

(defun get-scope (name)
  (let ((scope (gethash name all-scopes)))
    (DEBUG2! "get-scope %s"
	     (Object/to-string scope))
    scope))

(defun packages(features)
  (DEBUG! "Get packages for features: %s"
  	  features)
  ;;(unless features (edebug))
  (delete-dups
   (collect-lists nil
		  (cl-loop for f in features
			   collect (let ((pkgs nil))
				     (let ((feature (get-feature f)))
				       (when feature
					 (setf pkgs (Feature/pkglist feature))))
				     pkgs)))))

(defun load-module-definition (module-file)
  (load-file module-file))

(defun load-modules (dir)
  (let ((module-files (directory-files-recursively dir "modules.el$")))
    (cl-loop for module-file in module-files
	     do (load-module-definition module-file))))

(defun install-packages-for-scope (scope-name)
  (with-scope scope-name
	      scope
	      (Scope/install-pkgs scope)))

(defvar remote-autoload-pkgs nil
  "List of packages used in remote autoload")

;; autoload-r! will enabled a function in a package
;; which has not already been installed to be
;; autoloaded
(defmacro autoload-r! (fn pkgs filename &optional interactive)
  `(progn
     (defun ,fn (&rest args)
       ,(when interactive
	  `(interactive))
       ,@(cl-loop for pkg in pkgs
		  collect `(install-package-by-name ',pkg))
       (let ((my-self (symbol-function ',fn)))
	 (fmakunbound ',fn)
	 (if (load-library ,filename)
	     (apply ',fn args)
	   (fset ',fn my-self))))
     (setf remote-autoload-pkgs (append remote-autoload-pkgs ',pkgs))))

(defvar rmodes-alist nil
  "remote/local mode alist")

(defmacro rmode! (name docstring pkgs config-fn lmode)
  `(progn
     (mode! ,name ,docstring ,pkgs ,config-fn ,lmode)
     (setf rmodes-alist (plist-put rmodes-alist ',name ',lmode))))

(defun local-or-rmode-name (mode)
  (let ((m (plist-get rmodes-alist mode)))
    (if m
	m
      mode)))

(defmacro mode2feature! (mode feature pkgs docstring)
  `(let ((activate-fn (intern (concat "activate-" (symbol-name
						   ',feature))))
	 (deactivate-fn (intern (concat "deactivate-" (symbol-name
						       ',feature)))))
     `(progn
	(defun ,activate-fn ()
	  (,',mode))
	(defun ,deactivate-fn ()
	  (,',mode -1))
	(feature! ,',feature
		  ,',docstring
		  ,',pkgs
		  nil
		  ,activate-fn
		  ,deactivate-fn))))

(provide 'core-modules)
