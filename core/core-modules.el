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
   (pkg-info :initarg :pkg-info
	     :initform nil)
   (patches :initarg :patches
	    :initform nil)
   (installed :initarg :installed
	      :initform nil))
  "Class to describe the package of Emacs")

(defmethod Package/install ((pkg Package))
  (with-slots (name installed pkg-info patches) pkg
    (unless installed
      (when (fboundp 'install-pkg)
	(install-pkg pkg-info)
	(setf installed t)))))

(defmethod Package/apply_patches ((pkg Package))
  (with-slots (name patches) pkg
    (DEBUG! "Package/apply_patches %s"
	    pkg)
    (unless patches
      (setf patches (package-patches name)))
    (when patches
      (apply-package-patches name patches))))

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
    (let ((f (pcase (substring (symbol-name name) 0 1)
	       ("-" (progn
		      (setq options
			    (plist-put options
				       :status
				       -1))
		      (get-feature (intern
				    (substring
				     (symbol-name
				      name)
				     1)))))
	       ("+" (progn
		      (setq options
			    (plist-put options
				       :status
				       1))
		      (get-feature (intern
				    (substring
				     (symbol-name
				      name)
				     1)))))
	       (_ (progn
		    (get-feature name))))))
      (DEBUG! "invoke-feature %s f = %s options = %s"
	      name f options)
      (when f
	(pcase fn
	  ('prepare
	   (Feature/prepare f scope phase options))
	  ('configure
	   (Feature/configure f scope phase options))
	  ('activate
	   (progn
	     (cl-loop for pkg in (Feature/pkglist f scope options)
		      do (install-package-by-name pkg))
	     (Feature/activate f scope phase options)))
	  ('pkglist
	   (Feature/pkglist f scope options))
	  (_ nil))))))

(defun feature-off (feature options)
  (let ((status (plist-get options (mk-keyword (symbol-name feature)))))
    (and status
	(< status 0))))

(defun feature-on (feature options)
  (let ((status (plist-get options (mk-keyword (symbol-name feature)))))
    (and status
	 (> status 0))))

(defmethod Object/to-string ((obj Feature))
  (with-slots (name pkgs config-fn on-fn ) obj
    (format "Feature name: %s pkgs: %s config-fn: %s on-fn:%s "
	    name pkgs config-fn on-fn )))

(defvar all-packages (make-hash-table)
  "All defined packages")

(defvar all-features (make-hash-table)
  "All defined features")

(defvar all-modes (make-hash-table)
  "All defined models")

(defmacro package! (&rest args)
  `(let ((name (plist-get ',args :name))
	 (docstring (plist-get ',args :docstring))
	 (pkginfo (plist-get ',args :pkginfo)))
     (let ((patches (package-patches name)))
       (let ((package (make-instance 'Package
				     :name name
				     :docstring docstring
				     :pkg-info pkginfo
				     :patches patches
				     :installed nil)))
	 (progn
	   (puthash name package all-packages))))))

(defmacro package-ex! (name docstring pkginfo)
  (declare (doc-string 2))
  `(package! :name ,name
	     :docstring ,docstring
	     :pkginfo ,pkginfo))

(defun dummy-activate-fun (scope &optional phase options)
  (DEBUG! "dummy-activate-fun scope %s phase %s options %s"
	  scope phase options)
  t)

(defmacro feature-ex! (name docstring pkgs config-fn prepare-fn on-fn)
  (declare (doc-string 2))
  (unless on-fn
    (setq on-fn 'dummy-activate-fun))
  `(progn
     (defvar ,(intern (format "%s-before-activate-hook"
			      (symbol-name name)))
       nil)
     (defvar ,(intern (format "%s-after-activate-hook"
			      (symbol-name name)))
       nil)

     (defun ,(intern (format "call-%s/:activate"
			     (symbol-name name)))
	 (scope &optional phase options)
       (run-hooks ',(intern (format "%s-before-activate-hook"
				    (symbol-name name))))
       (,on-fn scope phase options)
       (run-hooks ',(intern (format "%s-after-activate-hook"
				    (symbol-name name)))))
     (let ((feature (make-instance 'Feature
				   :name ',name
				   :docstring ,docstring
				   :pkgs ',pkgs
				   :config-fn ',config-fn
				   :prepare-fn ',prepare-fn
				   :on-fn ',(intern (format
						     "call-%s/:activate"
						     (symbol-name name))))))
       (progn
	 (puthash ',name feature all-features)
	 (set (intern (concat (symbol-name ',name)
			      "-actived"))
	      nil)))))

(defmacro before-activate! (feature &rest body)
  `(add-hook ',(intern (format "%s-before-activate-hook"
			      (symbol-name feature)))
	     (lambda ()
	       ,@body)))

(defmacro after-activate! (feature &rest body)
  `(add-hook ',(intern (format "%s-after-activate-hook"
			      (symbol-name feature)))
	     (lambda ()
	       ,@body)))

(defmacro feature! (name docstring pkgs config-fn on-fn unused)
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

;; enable package patches when enable/install
;; packages
(add-hook 'straight-use-package-prepare-functions
	  #'(lambda (pkg-name)
	      (DEBUG! "prepare pkg %s" pkg-name)
	      (let ((pkg (get-package pkg-name)))
		(when pkg
		  (Package/apply_patches pkg)))))

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

(defvar mode-auto-feature-list nil
  "automatic feature for mode")

(defun auto-features (mode)
  (let ((key (mk-keyword mode)))
    (plist-get mode-auto-feature-list
	       key)))

(defun add-auto-features (mode &rest features)
  (let ((old-features (auto-features mode)))
    (setf mode-auto-feature-list
	  (plist-put mode-auto-feature-list
		     (mk-keyword mode)
		     (append old-features
			     features)))))

(provide 'core-modules)
