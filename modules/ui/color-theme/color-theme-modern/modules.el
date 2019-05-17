(package!
 color-theme-modern
 "color theme modern"
 (color-theme-modern :type git
		     :host github
		     :repo "emacs-jp/replace-colorthemes"))

(defvar color-theme-used 'billw
  "The default color theme will be used when this feature has been turned")

(defun enable-color-theme-modern ()
  (when color-theme-used
    (load-theme color-theme-used t t)
    (enable-theme color-theme-used)))

(feature!
 color-theme-modern
 "color theme modern"
 (color-theme-modern)
 nil
 enable-color-theme-modern
 nil)
