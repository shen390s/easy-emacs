(defvar custom-themes nil
  "A list of themes will be tried")

(defun gen-theme-list (theme)
  (let ((name (symbol-name theme)))
    (list (intern (format "%s-x%d"
     		  name (x-display-pixel-width)))
	  theme)))

(defun load-theme-ex (theme flag)
  (DEBUG! "trying to loading theme %s" theme)
  (condition-case err
      (load-theme theme flag)
    (error (DEBUG! "%s" (error-message-string err))
	   nil)))

(defun load-themes (themes flag)
  (DEBUG! "loading themes %s flag %s"
	  themes flag)
  (when themes
    (unless (load-theme-ex (car themes) flag)
      (load-themes (cdr themes) flag))))

(defun activate-load-custom (scope &optional phase options)
  (DEBUG! "activate-load-custom scope %s phase %s options %s"
	  scope phase options)
  (let ((theme (plist-get options :theme)))
    (when theme
      (setq custom-themes (gen-theme-list theme)))
    (DEBUG! "custom-themes %s" custom-themes)
    (DEBUG! "custom-file %s" custom-file)
    (load custom-file t t)
    (when custom-themes
      (load-themes custom-themes t))))

(feature-ex! load-custom
	     "load customization file"
	     nil
	     nil
	     nil
	     activate-load-custom)
