(defvar use-polymode nil
  "enable poly mode")

(defmacro activate-settings (&rest vars)
  `(set-vars ,@vars))

(feature! settings
	  "setting of settingsment"
          nil
          nil
          activate-settings
          nil)
