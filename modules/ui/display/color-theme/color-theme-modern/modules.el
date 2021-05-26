(package! color-theme-modern
	     "color theme modern"
	     (color-theme-modern :type git
				 :host github
				 :repo "emacs-jp/replace-colorthemes"))

(defvar color-theme-used 'billw
  "The default color theme will be used when this feature has been turned")

(defun activate-color-theme-modern (scope &optional phase options)
  (pcase scope
    ('ui (let ((status (plist-get options :status)))
	   (when (and status
		      (>= status 0))
	     (when color-theme-used
	       (load-theme color-theme-used t t)
	       (enable-theme color-theme-used)))))
    (_ t)))

(feature! color-theme-modern
	     "color theme modern"
	     (color-theme-modern)
	     nil
	     nil
	     activate-color-theme-modern)
