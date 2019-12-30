(defun config-load-custom ()
  t)

(defun enable-load-custom (&optional theme)
  (load custom-file t t)
  (when theme
      (load-theme theme t)))

(feature! load-custom
	  "load customization file"
	  nil
	  config-load-custom
	  enable-load-custom
	  nil)
