
(package! :name go-mode
	  :docstring "A major Emacs mode for edit Go source code"
	  :pkginfo (go-mode :type git
			    :host github
			    :repo "dominikh/go-mode.el"))

(autoload-r! go-mode
	     (go-mode)
	     "go-mode"
	     t)

