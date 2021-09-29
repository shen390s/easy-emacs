;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'core-lib)
(require 'core-log)
(require 'core-package)

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

(cl-defmethod Feature/prepare ((feature Feature) 
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


(cl-defmethod Feature/configure ((feature Feature) 
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

(cl-defmethod Feature/activate ((feature Feature) 
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

(cl-defmethod Feature/pkglist ((feature Feature) &optional scope-name config-options)
  (with-slots (pkgs) feature
    (let ((zpkgs (if (functionp pkgs)
		     (funcall pkgs scope-name config-options)
		   pkgs)))
      (if (listp zpkgs)
	  zpkgs
	(list zpkgs)))))

(defvar-local easy-emacs-buffer-features nil
  "feature setting activate/disabled for easy emacs buffer")

(defun invoke-feature (name fn scope phase options)
  (DEBUG! "invoke-feature %s fn = %s scope = %s phase = %s options %s"
	  name fn scope phase (pp-to-string options))
  (when name
    (let ((rname (pcase (substring (symbol-name name) 0 1)
		   ("-" (progn
			  (setq options
				(plist-put options
					   :status
					   -1))
			  (substring (symbol-name name)
				     1)))
		   ("+" (progn
			  (setq options
				(plist-put options
					   :status
					   1))
			  (substring (symbol-name name)
				     1)))
		   (_ (symbol-name name)))))
      (let ((f (get-feature (intern rname))))
	(DEBUG! "invoke-feature %s f = %s options = %s"
		name f (pp-to-string options))
	(when f
	  (pcase fn
	    ('prepare
	     (Feature/prepare f scope phase options))
	    ('configure
	     (Feature/configure f scope phase options))
	    ('activate
	     (progn
	       (let ((pkgs (Feature/pkglist f scope options)))
		 (when pkgs
		   (install-packages pkgs)))
	       ;; setup setting of buffer features
	       (setq easy-emacs-buffer-features
		     (plist-put easy-emacs-buffer-features
				(mk-keyword rname)
				(plist-get options
					   :status)))
	       (Feature/activate f scope phase options)))
	    ('pkglist
	     (Feature/pkglist f scope options))
	    (_ nil)))))))

(defun activate-feature (name scope options)
  (invoke-feature name 'activate scope
		  'ignore options))

(defun configure-feature (name scope phase options)
  (invoke-feature name 'configure scope
		  phase options))

(defun prepare-feature (name scope phase options)
  (invoke-feature name 'prepare scope
		  phase options))

(defun get-mode-from-options (options)
  (let ((mode  (intern (format "%s-mode"
			       (plist-get options :mode)))))
    (DEBUG! "get-mode-from-options options %s mode %s"
	    options mode)
    (if mode
	mode
      (ERR! "get-mode-from-options get null mode from option %s"
	    options))))

(defun feature-off-in-options (feature options)
  (let ((status (plist-get options (mk-keyword (symbol-name feature)))))
    (and status
	(< status 0))))

(defun feature-on-in-options (feature options)
  (let ((status (plist-get options (mk-keyword (symbol-name feature)))))
    (and status
	 (>= status 0))))

;; check whether feature has been on
;; in buffer

(defun feature-on (feature options)
  (DEBUG! "feature-on buffer %s feature %s options %s buffer-features %s"
	  (buffer-name) feature options
	  easy-emacs-buffer-features)
  (let ((status (plist-get options feature)))
    (if status
	(>= status 0)
      (let ((status (plist-get easy-emacs-buffer-features
			       feature)))
	(and status
	     (>= status 0))))))

(defun feature-off (feature options)
  (not (feature-on feature options)))

(cl-defmethod Object/to-string ((obj Feature))
  (pp-to-string obj))

(defun dummy-activate-fun (scope &optional phase options)
  (DEBUG! "dummy-activate-fun scope %s phase %s options %s"
	  scope phase options)
  t)

(defmacro feature! (name docstring pkgs config-fn prepare-fn on-fn)
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

(defun get-feature (f)
  (let ((feature (gethash f all-features)))
    (if feature
	(DEBUG2! "get-feature : %s"
		 (Object/to-string feature))
      (DEBUG! "get-feature %s not found"
	      f ))
    feature))

(defun packages(features &optional scope-name config-options)
  (DEBUG! "Get packages for features: %s scope %s options %s"
  	  features scope-name config-options)
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
		  collect `(install-package ',pkg))
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
