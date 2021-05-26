(package-ex! color-theme-solarized
	  "Solarized is a sixteen color palette (eight monotones, 
eight accent colors) designed for use with terminal and gui 
applications."
	  (color-theme-solarized :type  git
				 :host github
				 :repo "sellout/emacs-color-theme-solarized"))


(defun activate-color-theme-solarized (scope &optional phase options)
  (pcase scope
    ('ui (let ((status (plist-get options :status)))
	   (when (and status
		      (>= status 0))
	     (load-theme 'solarized t))))
    (_ t)))

(feature-ex! color-theme-solarized
	     "Solarized is a sixteen color palette (eight monotones,
 eight accent colors) designed for use with terminal and gui 
applications."
	     (color-theme-solarized)
	     nil
	     nil
	     activate-color-theme-solarized)
