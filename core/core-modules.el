(require 'cl-lib)
(require 'subr-x)

(cl-defstruct xpackage name docstring pkg-info)

(cl-defstruct xfeature name docstring pkgname check-fn on-fn off-fn)

(defvar all-xpackages (make-hash-table)
  "All defined packages")

(defvar all-xfeatures (make-hash-table)
  "All defined features")

(defun build-xfeature (pkgname name docstring check-fn on-fn off-fn)
  (let ((xfeature (make-xfeature :name `,name
				 :docstring docstring
				 :pkgname pkgname
				 :check-fn check-fn
				 :on-fn on-fn
				 :off-fn off-fn)))
    xfeature))

(defun build-xfeatures (pkgname xfeatures)
  (cl-loop for xfeature in xfeatures
	collect (apply #'build-xfeature (cons pkgname xfeature))))

(defun add-xpackage (xpackage)
  (puthash (xpackage-name xpackage) xpackage all-xpackages))

(defun add-xfeature (xfeature)
  (puthash (xfeature-name xfeature) xfeature all-xfeatures))

(defun add-xfeatures (xfeatures)
  (when xfeatures
    (let ((xfeature (car xfeatures))
	      (remain (cdr xfeatures)))
      (progn
	    (add-xfeature xfeature)
	    (add-xfeatures remain)))))

(defmacro package! (name docstring pkginfo xfeature-list)
  (let ((xpackage (make-xpackage :name `,name
				 :docstring docstring
				 :pkg-info pkginfo))
	(xfeatures (build-xfeatures `,name xfeature-list)))
    (progn
       (add-xpackage `,xpackage)
       (add-xfeatures `,xfeatures))))


(defun actived-packages(activated-features)
  (let ((feature-info (hash-table-values all-xfeatures)))
    (let ((feature-pkg-map (mapcar #'(lambda (xfeature)
				       (cons (xfeature-name xfeature)
					     (xfeature-pkgname xfeature)))
				   feature-info)))
      (mapcar #'(lambda (feature-name)
		  (cdr (assoc feature-name feature-pkg-map)))
	      activated-features))))

(defun pkglist-info (packages)
  (cl-loop for pkg in packages
	   collect (xpackage-pkg-info (gethash pkg all-xpackages))))
		
						      
(defun load-module-definition (module-file)
  (load-file module-file))

(defun load-modules (dir)
  (let ((module-files (directory-files-recursively dir "modules.el")))
    (cl-loop for module-file in module-files
	     do (load-module-definition module-file))))

(provide 'core-modules)
