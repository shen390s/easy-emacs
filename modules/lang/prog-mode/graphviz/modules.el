
(package-ex! graphviz
	     "Emacs mode for the graphviz"
	     (graphviz-dot-mode :type git
				:host github
				:repo "ppareit/graphviz-dot-mode"))


(autoload-r! graphviz-dot-mode
	     (graphviz)
	     "graphviz-dot-mode"
	     t)

