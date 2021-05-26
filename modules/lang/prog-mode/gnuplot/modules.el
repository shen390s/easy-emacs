
(package-ex! gnuplot-mode
	     "A major Emacs mode for edit gnuplot document"
	     (gnuplot-mode :type git
			   :host github
			   :repo "emacsorphanage/gnuplot"))

(autoload-r! gnuplot-mode
	     (gnuplot-mode)
	     "gnuplot"
	     t)

