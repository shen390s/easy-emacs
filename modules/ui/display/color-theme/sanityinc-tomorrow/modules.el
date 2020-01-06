(package! :name sanityinc-tomorrow
	  :docstring "A set of comprehensive Emacs color themes based on Chris Kempson's 'tomorrow' themes"
	  :pkginfo (sanityinc-tomorrow :type git
				       :host github
				       :repo "purcell/color-theme-sanityinc-tomorrow"))

(defun config-sanityinc-tomorrow ()
  t)

(defmacro  activate-sanityinc-tomorrow (actived)
  `(funcall (intern (concat "color-theme-sanityinc-tomorrow-"
			    (symbol-name ,actived)))))

(defun enable-sanityinc-tomorrow (actived-theme)
  (require 'color-theme-sanityinc-tomorrow)
  (activate-sanityinc-tomorrow actived-theme))

(feature! sanityinc-tomorrow
	  "A set of comprehensive Emacs color themes based on Chris Kempson's 'tomorrow' themes"
	  (sanityinc-tomorrow)
	  config-sanityinc-tomorrow
	  enable-sanityinc-tomorrow
	  nil)
