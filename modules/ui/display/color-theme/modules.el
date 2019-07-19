(defun config-color-theme ()
  t)

(defun activate-color-theme ()
  t)
  
(feature! color-theme
	  "choose color theme"
          nil
          config-color-theme
          activate-color-theme
          nil)
