(package! smart-mode-line
	     "A powerful and beautiful mode-line for Emacs. "
	     (smart-mode-line :type git
			      :host github
			      :repo "Malabarba/smart-mode-line"))

(defun activate-smart-mode-line (scope &optional phase options)
  (require 'smart-mode-line)
  (sml/setup))

(feature! smart-mode-line
	     "A powerful and beautiful mode-line for Emacs."
	     (smart-mode-line)
	     nil
	     nil
	     activate-smart-mode-line)
