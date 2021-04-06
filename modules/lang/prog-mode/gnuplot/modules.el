
(package! :name gnuplot-mode
	  :docstring "A major Emacs mode for edit gnuplot document"
	  :pkginfo (gnuplot-mode :type git
				 :host github
				 :repo "emacsorphanage/gnuplot"))

(defun config-gnuplot ()
  (DEBUG! "configuring gnuplot mode...")
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.gp\\'" . lang/gnuplot-mode))
    t))

(autoload-r! gnuplot-mode
	     (gnuplot-mode)
	     "gnuplot"
	     t)

(rmode! lang/gnuplot-mode
	"Emacs mode for gnuplot"
	(gnuplot-mode)
	config-gnuplot
	gnuplot-mode)

