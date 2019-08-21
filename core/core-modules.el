;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'core-lib)
(require 'core-log)
(require 'core-package)

(cl-defstruct xpackage name docstring pkg-info installed)

(cl-defstruct xfeature name docstring pkgs config-fn on-fn off-fn)

(defvar all-xpackages (make-hash-table)
  "All defined packages")

(defvar all-xfeatures (make-hash-table)
  "All defined features")

(defmacro package! (name docstring pkginfo)
  `(let ((xpackage (make-xpackage :name ',name
				  :docstring ,docstring
				  :pkg-info ',pkginfo
				  :installed nil)))
     (progn
       (puthash ',name xpackage all-xpackages))))

(defmacro feature! (name docstring pkgs config-fn on-fn off-fn)
  `(let ((xfeature (make-xfeature :name ',name
				  :docstring ,docstring
				  :pkgs ',pkgs
				  :config-fn ',config-fn
				  :on-fn ',on-fn
				  :off-fn ',off-fn)))
     (progn
       (puthash ',name xfeature all-xfeatures))))

(cl-defmethod install-xpackage ((pkg xpackage))
  (unless (xpackage-installed pkg)
    (if (fboundp 'install-pkg)
	(install-pkg (xpackage-pkg-info pkg)))
    (setf (xpackage-installed pkg) t)))


(defun install-package-by-name (pkg)
  (DEBUG! "installing package %s..." pkg)
  (let ((xpackage (gethash pkg all-xpackages)))
    (when xpackage
      (install-xpackage xpackage))))

(cl-defmethod config-xfeature ((f xfeature))
  (let ((config-fn (xfeature-config-fn f)))
    (if config-fn
	(condition-case err
	    (let ((result (funcall config-fn)))
	      (DEBUG! "config xfeature %s get %s"
		      f result)
	      (unless result
		(WARN! "configure %s failed" f))
	      result)
	  (error (WARN! "configure xfeature %s error %s"
			f (error-message-string err))
		 nil))
      t)))

(cl-defmethod pkglist-xfeature ((f xfeature))
  (let ((xpkgs (xfeature-pkgs f)))
    (let ((pkgs (if (functionp xpkgs)
		    (funcall xpkgs)
		  xpkgs)))
      (if (listp pkgs)
	  pkgs
	(list pkgs)))))

(defun actived-packages(activated-features)
  (DEBUG! "Get actived packages for features: %s"
	  activated-features)
  (let ((feature-info (hash-table-values all-xfeatures)))
    (let ((feature-pkg-map (mapcar #'(lambda (xfeature)
				       (cons (xfeature-name xfeature)
					     (pkglist-xfeature xfeature)))
				   feature-info)))
      (delete-dups
       (collect-lists nil
		      (mapcar #'(lambda (feature-name)
				  (cdr (assoc feature-name feature-pkg-map)))
			      activated-features))))))
						      
(defun load-module-definition (module-file)
  (load-file module-file))

(defun load-modules (dir)
  (let ((module-files (directory-files-recursively dir "modules.el$")))
    (cl-loop for module-file in module-files
	     do (load-module-definition module-file))))

(provide 'core-modules)
