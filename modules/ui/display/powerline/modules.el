(package! :name powerline
	  :docstring "Emacs version of the VIM powerline"
	  :pkginfo (powerline :type git
			      :host github
			      :repo "milkypostman/powerline"))

(defun enable-powerline ()
  (require 'powerline)
  (powerline-default-theme))

(feature!
 powerline
 "Emacs version of the VIM powerline"
 (powerline)
 nil
 enable-powerline
 nil)
