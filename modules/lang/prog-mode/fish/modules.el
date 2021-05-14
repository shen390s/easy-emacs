
(package-ex! fish-mode
	     "A major Emacs mode for edit Fish source code"
	     (fish-mode :type git
			:host github
			:repo "wwwjfy/emacs-fish"))

(autoload-r! fish-mode
	     (fish-mode)
	     "fish-mode"
	     t)


