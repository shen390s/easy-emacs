;;

(require  'cl-lib)

(defvar enabled-features nil
  "Enabled features")

(defmacro enable! (feature)
	t)

(defun enable-features (features)
  t)

(provide 'core-features)
