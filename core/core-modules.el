(require 'cl-lib)

(cl-defstruct module name docstring pkg-info features)

(defvar modules nil
  "All available modules")

(defmacro module! (name docstring pkg-info features)
  `(let ((module (make-module :name ',name
			      :docstring ,docstring
			      :pkg-info ',pkg-info
			      :features ',features)))
    (setf modules (cons module modules))))

(defun load-module-definition (module)
  (message "loading definition module from %s..." module)
  (load-file module))

(defun load-modules (dir)
  (let ((modules (directory-files-recursively dir "modules.el")))
    (cl-loop for module in modules
	     do (load-module-definition module))))

(provide 'core-modules)
