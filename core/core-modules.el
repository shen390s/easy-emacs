;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct xpackage name docstring pkg-info)

(cl-defstruct xfeature name docstring pkgs config-fn on-fn off-fn)

(defvar all-xpackages (make-hash-table)
  "All defined packages")

(defvar all-xfeatures (make-hash-table)
  "All defined features")

(defmacro package! (name docstring pkginfo)
  `(let ((xpackage (make-xpackage :name ',name
				  :docstring ,docstring
				  :pkg-info ',pkginfo)))
     (progn
       (let ((old-xpackage (gethash ',name all-xpackages)))
	 (when old-xpackage
	   (message "Warning: duplicated package with name `%s'"
		    ',name)))
       (puthash ',name xpackage all-xpackages))))

(defmacro feature! (name docstring pkgs config-fn on-fn off-fn)
  `(let ((xfeature (make-xfeature :name ',name
				  :docstring ,docstring
				  :pkgs ',pkgs
				  :config-fn ',config-fn
				  :on-fn ',on-fn
				  :off-fn ',off-fn)))
     (progn
       (let ((old-xfeature (gethash ',name all-xpackages)))
	 (when old-xfeature
	   (message "Warning: duplicated feature with name `%s'"
		    ',name)))
       (puthash ',name xfeature all-xfeatures))))

(defun config-xfeature (xfeature)
  (let ((config-fn (xfeature-config-fn xfeature)))
    (if config-fn
	(funcall config-fn)
      t)))

(defun extract-xfeature-pkgs (xfeature)
  (let ((xpkgs (xfeature-pkgs xfeature)))
    (let ((pkgs (if (functionp xpkgs)
		    (funcall xpkgs)
		  xpkgs)))
      (if (listp pkgs)
	  pkgs
	(list pkgs)))))

(defun actived-packages(activated-features)
  (let ((feature-info (hash-table-values all-xfeatures)))
    (let ((feature-pkg-map (mapcar #'(lambda (xfeature)
				       (cons (xfeature-name xfeature)
					     (extract-xfeature-pkgs xfeature)))
				   feature-info)))
      (delete-dups
       (collect-lists nil
		      (mapcar #'(lambda (feature-name)
				  (cdr (assoc feature-name feature-pkg-map)))
			      activated-features))))))

(defun pkglist-info (packages)
  (cl-loop for pkg in packages
	   collect (xpackage-pkg-info (gethash pkg all-xpackages))))
		
						      
(defun load-module-definition (module-file)
  (load-file module-file))

(defun load-modules (dir)
  (let ((module-files (directory-files-recursively dir "modules.el$")))
    (cl-loop for module-file in module-files
	     do (load-module-definition module-file))))

(provide 'core-modules)
