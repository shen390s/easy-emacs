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

(defmacro module! (name docstring pkg-info features)
  (let ((module (make-module :name `',name
			     :docstring docstring
			     :pkg-info pkg-info
			     :features (build-features features))))
    `(setf modules (cons ,module modules))))

(defun load-module-definition (module)
  (message "loading definition module from %s..." module)
  (load-file module))

(defun load-modules (dir)
  (let ((modules (directory-files-recursively dir "modules.el")))
    (cl-loop for module in modules
	     do (load-module-definition module))))

(provide 'core-modules)
