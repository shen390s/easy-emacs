(defun config-load-custom ()
  t)

(defun enable-load-custom (theme)
  (load custom-file t t)
  (when theme
      (load-theme theme)))

(feature! load-custom
	  "load customization file"
	  nil
	  config-load-custom
	  enable-load-custom
	  nil)
