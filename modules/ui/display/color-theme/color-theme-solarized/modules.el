(package! :name color-theme-solarized
	  :docstring "Solarized is a sixteen color palette (eight monotones, eight accent colors) designed for use with terminal and gui applications."
	  :pkginfo (color-theme-solarized :type  git
					  :host github
					  :repo "sellout/emacs-color-theme-solarized"))


(defun enable-color-theme-solarized ()
  (load-theme 'solarized t))

(feature!
 color-theme-solarized
 "Solarized is a sixteen color palette (eight monotones, eight accent colors) designed for use with terminal and gui applications."
 (color-theme-solarized)
 nil
 enable-color-theme-solarized
 nil)
