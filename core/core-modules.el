(require 'cl-lib)

(cl-defstruct package name docstring pkg-info)

(cl-defstruct feature name docstring pkgname check-fn on-fn off-fn)

(defvar all-packages (make-hash-table)
  "All defined packages")

(defvar all-features (make-hash-table)
  "All defined features")

(defun build-feature (pkgname name docstring check-fn on-fn off-fn)
  (make-feature :name name
		:docstring docstring
		:pkgname pkgname
		:check-fn check-fn
		:on-fn on-fn
		:off-fn off-fn))

(defun build-features (pkgname features)
  (cl-loop for feature in features
	collect (apply #'build-feature (cons pkgname feature))))

(defun add-package (package)
  (puthash (package-name package) package all-packages))

(defun add-feature (feature)
  (puthash (feature-name feature) feature all-features))

(defmacro package! (name docstring pkginfo feature-list)
  (let ((package (make-package :name `,name
			       :docstring docstring
			       :pkg-info pkginfo))
	(features (build-features `,name feature-list)))
    `(progn
       (add-package ,package)
       (cl-loop for feature in ,features
		do (add-feature feature)))))

(defun load-module-definition (module-file)
  (load-file module-file))

(defun load-modules (dir)
  (let ((module-files (directory-files-recursively dir "modules.el")))
    (cl-loop for module-file in module-files
	     do (load-module-definition module-file))))

(provide 'core-modules)
