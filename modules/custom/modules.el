;; -*- lexical-binding: t -*-
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

(defun config-load-custom (scope &optional phase options)
  (pcase phase
    (:pre-check (setq sml/no-confirm-load-theme t))
    (_ t)))

(defun activate-load-custom (scope &optional phase options)
  (DEBUG! "activate-load-custom scope %s phase %s options %s"
	  scope phase options)
  (let ((themes (plist-get options :theme)))
    (when themes
      (setq custom-themes
	    (collect-lists nil
			   (cl-loop for theme in themes
				    collect (gen-theme-list theme)))))
    (DEBUG! "custom-themes %s" custom-themes)
    (DEBUG! "custom-file %s" custom-file)
    (load custom-file t t)
    (when custom-themes
      (load-themes custom-themes t))))

(feature! load-custom
	     "load customization file"
	     nil
	     config-load-custom
	     nil
	     activate-load-custom)
