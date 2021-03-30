(defvar custom-themes nil
  "A list of themes will be tried")

(defun config-load-custom ()
  t)

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
  (when themes
    (unless (load-theme-ex (car themes) flag)
      (load-themes (cdr themes) flag))))

(defun custom-global-scope-enter-hook ()
  (load custom-file t t)
  (when custom-themes
    (load-themes custom-themes t)))

(defun enable-load-custom (&optional theme)
  (when theme
    (setq custom-themes (gen-theme-list theme)))
  (add-scope-hook 'global
		  'after
		  'custom-global-scope-enter-hook))

(feature! load-custom
	  "load customization file"
	  nil
	  config-load-custom
	  enable-load-custom
	  nil)
