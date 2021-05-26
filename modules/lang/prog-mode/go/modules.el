
(package! go-mode
	     "A major Emacs mode for edit Go source code"
	     (go-mode :type git
		      :host github
		      :repo "dominikh/go-mode.el"))

(autoload-r! go-mode
	     (go-mode)
	     "go-mode"
	     t)

