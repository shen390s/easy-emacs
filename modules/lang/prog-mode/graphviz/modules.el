
(package! :name graphviz
	  :docstring "Emacs mode for the graphviz"
	  :pkginfo (graphviz-dot-mode :type git
				      :host github
				      :repo "ppareit/graphviz-dot-mode"))


(autoload-r! graphviz-dot-mode
	     (graphviz)
	     "graphviz-dot-mode"
	     t)

