(package! :name smart-mode-line
	  :docstring "A powerful and beautiful mode-line for Emacs. "
	  :pkginfo (smart-mode-line :type git
				    :host github
				    :repo "Malabarba/smart-mode-line"))

(defun enable-smart-mode-line ()
  (require 'smart-mode-line)
  (sml/setup))

(feature! smart-mode-line
	  "A powerful and beautiful mode-line for Emacs."
	  (smart-mode-line)
	  nil
	  enable-smart-mode-line
	  nil)
