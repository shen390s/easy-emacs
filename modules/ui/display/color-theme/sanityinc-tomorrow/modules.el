(package! sanityinc-tomorrow
	  "A set of comprehensive Emacs color themes based on Chris Kempson's 'tomorrow' themes"
	  (sanityinc-tomorrow :type git
			      :host github
			      :repo "purcell/color-theme-sanityinc-tomorrow"))

(defvar sanityinc-tomorrow-actived 'blue
  "Activated sanityinc-tomorrow theme")

(defun config-sanityinc-tomorrow ()
  t)

(defmacro  activate-sanityinc-tomorrow ()
  `(,(intern (concat "color-theme-sanityinc-tomorrow-"
		     (symbol-name sanityinc-tomorrow-actived)))))

(defun enable-sanityinc-tomorrow ()
  (require 'color-theme-sanityinc-tomorrow)
  (activate-sanityinc-tomorrow))

(feature! sanityinc-tomorrow
	  "A set of comprehensive Emacs color themes based on Chris Kempson's 'tomorrow' themes"
	  (sanityinc-tomorrow)
	  config-sanityinc-tomorrow
	  enable-sanityinc-tomorrow
	  nil)
