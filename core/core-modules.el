(defmacro module! (name docstring pkg-info features)
  t)

(defun load-module-definition (module)
  (message "loading module information from %s..." module))

(defun load-modules (dir)
  (let ((modules (directory-files-recursively dir "modules.el")))
    (cl-loop for module in modules
	     do (load-module-definition module))))

(provide 'core-modules)
