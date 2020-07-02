(defun config-load-custom ()
  t)

(defun gen-theme-list (theme)
  (let ((name (symbol-name theme)))
    (list (intern (format "%s-x%d"
     		  name (x-display-pixel-width)))
	  theme)))

(defun load-theme-ex (theme flag)
  (INFO! "Loading theme %s" theme)
  (if (custom-theme-p theme)
      (load-theme theme flag)
    nil))

(defun load-themes (themes flag)
  (when themes
    (unless (load-theme-ex (car themes) flag)
      (load-themes (cdr themes) flag))))

(defun enable-load-custom (&optional theme)
  (load custom-file t t)
  (when theme
      (load-themes (gen-theme-list theme) t)))

(feature! load-custom
	  "load customization file"
	  nil
	  config-load-custom
	  enable-load-custom
	  nil)
