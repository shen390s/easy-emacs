(require 'cl-lib)
(require 'subr-x)

(cl-defstruct xpackage name docstring pkg-info)

(cl-defstruct xfeature name docstring pkgs check-fn on-fn off-fn)

(defvar all-xpackages (make-hash-table)
  "All defined packages")

(defvar all-xfeatures (make-hash-table)
  "All defined features")

(defmacro package! (name docstring pkginfo)
  (let ((xpackage (make-xpackage :name `,name
				 :docstring docstring
				 :pkg-info pkginfo)))
    (progn
      (puthash name xpackage all-xpackages))))

(defmacro feature! (name docstring pkgs check-fn on-fn off-fn)
  (let ((xfeature (make-xfeature :name `,name
				 :docstring docstring
				 :pkgs pkgs
				 :check-fn check-fn
				 :on-fn on-fn
				 :off-fn off-fn)))
    (puthash name xfeature all-xfeatures)))

(defun actived-packages(activated-features)
  (let ((feature-info (hash-table-values all-xfeatures)))
    (let ((feature-pkg-map (mapcar #'(lambda (xfeature)
				       (cons (xfeature-name xfeature)
					     (xfeature-pkgs xfeature)))
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
