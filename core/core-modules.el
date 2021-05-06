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
  (with-slots (installed pkg-info) pkg
    (unless installed
      (when (fboundp 'install-pkg)
	(install-pkg pkg-info)
	(setf installed t)))))

(defmethod Object/to-string ((obj Package))
  (with-slots (name pkg-info installed) obj
      (format "Package name: %s pkginfo: %s installed: %s"
	      name pkg-info installed)))

(defclass Feature ()
  ((name :initarg :name
	 :initform "Anonymous")
   (docstring :initarg :docstring
	      :initform "Anonymous")
   (pkgs :initarg :pkgs
	 :initform nil)
   (prepare-fn :initarg :prepare-fn
	       :initform nil)
   (config-fn :initarg :config-fn
	      :initform nil)
   (on-fn :initarg :on-fn
	  :initform nil)
   (off-fn :initarg :off-fn
	   :initform nil))
  "Class to describe the feature of Emacs")

(defmethod Feature/prepare ((feature Feature) 
			      &optional scope-name
			      phase config-options)
  (let ((result t))
    (with-slots (prepare-fn) feature
      (when prepare-fn
	(condition-case err
	    (setf result (funcall prepare-fn scope-name phase config-options))
	  (error (WARN! "prepare feature %s error %s"
			(Object/to-string feature) (error-message-string err))
		 nil))))
    (DEBUG2! "prepare feature %s return %s in scope %s phase %s config options %s"
	     (Object/to-string feature) result
	     scope-name phase config-options)
    result))


(defmethod Feature/configure ((feature Feature) 
			      &optional scope-name
			      phase config-options)
  (let ((result t))
    (with-slots (config-fn) feature
      (when config-fn
	(condition-case err
	    (setf result (funcall config-fn scope-name phase config-options))
	  (error (WARN! "configure feature %s error %s"
			(Object/to-string feature) (error-message-string err))
		 nil))))
    (DEBUG2! "configure feature %s return %s in scope %s phase %s config options %s"
	     (Object/to-string feature) result
	     scope-name phase config-options)
    result))

(defmethod Feature/activate ((feature Feature) 
			     &optional scope-name
			     phase config-options)
  (let ((result t))
    (with-slots (on-fn) feature
      (when on-fn
	(condition-case err
	    (setf result (funcall on-fn scope-name phase config-options))
	  (error (WARN! "activate feature %s error %s"
			(Object/to-string feature) (error-message-string err))
		 nil))))
    (DEBUG2! "activate feature %s return %s in scope %s phase %s config options %s"
	     (Object/to-string feature) result
	     scope-name phase config-options)
    result))

(defmethod Feature/pkglist ((feature Feature) &optional scope-name config-options)
  (with-slots (pkgs) feature
    (let ((zpkgs (if (functionp pkgs)
		     (funcall pkgs scope-name config-options)
		   pkgs)))
      (if (listp zpkgs)
	  zpkgs
	(list zpkgs)))))

(defun invoke-feature (name fn scope phase options)
  (DEBUG! "invoke-feature %s fn = %s scope = %s phase = %s options %s"
	  name fn scope phase options)
  (when name
    (let ((f (get-feature name)))
      (when f
	(pcase fn
	  ('prepare
	   (Feature/prepare f scope phase options))
	  ('configure
	   (Feature/configure f scope phase options))
	  ('activate
	   (progn
	     (cl-loop for pkg in (Feature/pkglist f scope options)
		      do (Package/install (get-package pkg)))
	     (Feature/activate f scope phase options)))
	  ('pkglist
	   (Feature/pkglist f scope options))
	  (_ nil))))))

(defmethod Object/to-string ((obj Feature))
  (with-slots (name pkgs config-fn on-fn off-fn) obj
    (format "Feature name: %s pkgs: %s config-fn: %s on-fn:%s off-fn:%s"
	    name pkgs config-fn on-fn off-fn)))

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
  (with-slots (name pkgs on-fn) obj
    (format "Mode name: %s pkgs: %s on-fn: %s"
	    name pkgs on-fn)))

(defvar all-packages (make-hash-table)
  "All defined packages")

(defvar all-features (make-hash-table)
  "All defined features")

(defvar all-modes (make-hash-table)
  "All defined models")

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

(defmacro feature-ex! (name docstring pkgs config-fn prepare-fn on-fn)
  `(let ((feature (make-instance 'Feature
				 :name ',name
				 :docstring ,docstring
				 :pkgs ',pkgs
				 :config-fn ',config-fn
				 :prepare-fn ',prepare-fn
				 :on-fn ',on-fn)))
     (progn
       (puthash ',name feature all-features)
       (set (intern (concat (symbol-name ',name)
			    "-actived"))
	    nil))))

(defmacro feature! (name docstring pkgs config-fn on-fn off-fn)
  `(let ((feature (make-instance 'Feature
				 :name ',name
				 :docstring ,docstring
				 :pkgs ',pkgs
				 :config-fn ',config-fn
				 :on-fn ',on-fn)))
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

(defun packages(features &optional scope-name config-options)
  (DEBUG! "Get packages for features: %s"
  	  features)
  ;;(unless features (edebug))
  (delete-dups
   (collect-lists nil
		  (cl-loop for f in features
			   collect (invoke-feature f
						   'pkglist
						   scope-name
						   'ignore
						   config-options)))))

(defun load-module-definition (module-file)
  (load-file module-file))

(defun load-modules (dir)
  (let ((module-files (directory-files-recursively dir "modules.el$")))
    (cl-loop for module-file in module-files
	     do (load-module-definition module-file))))


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
