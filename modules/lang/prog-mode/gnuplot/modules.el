
(package! :name gnuplot-mode
	  :docstring "A major Emacs mode for edit gnuplot document"
	  :pkginfo (gnuplot-mode :type git
				 :host github
				 :repo "emacsorphanage/gnuplot"))

(autoload-r! gnuplot-mode
	     (gnuplot-mode)
	     "gnuplot"
	     t)

