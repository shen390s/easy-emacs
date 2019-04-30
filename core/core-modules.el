(require 'cl-lib)

(cl-defstruct module name docstring pkg-info features)

(cl-defstruct feature name check-fn on-fn off-fn)

(defvar modules nil
  "All available modules")

(defun build-feature (name check-fn on-fn off-fn)
  (make-feature :name name
		:check-fn check-fn
		:on-fn on-fn
		:off-fn off-fn))

(defun build-features (features)
  (cl-loop for feature in features
	collect (apply #'build-feature feature)))

(defun add-module (module)
  (setf modules (cons module modules)))

(defmacro module! (name docstring pkg-info features)
  (let ((module (make-module :name `',name
			     :docstring docstring
			     :pkg-info pkg-info
			     :features (build-features features))))
    `(progn
       (add-module ,module))))

(defun load-module-definition (module-file)
  (load-file module-file))

(defun load-modules (dir)
  (let ((module-files (directory-files-recursively dir "modules.el")))
    (cl-loop for module-file in module-files
	     do (load-module-definition module-file))))

(provide 'core-modules)
