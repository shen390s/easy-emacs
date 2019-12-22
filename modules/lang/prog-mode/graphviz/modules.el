(scope! graphviz prog-lang )

(package! graphviz
	  "Emacs mode for the graphviz"
	  (graphviz-dot-mode :type git
			     :host github
			     :repo "ppareit/graphviz-dot-mode"))

(defun config-graphviz ()
  (progn
    (DEBUG! "configuring graphviz mode")
    (add-to-list 'auto-mode-alist
		 '("\\.dot\\'" . graphviz-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.diag\\'" . graphviz-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.gv\\'" . graphviz-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.blockdiag\\'" . graphviz-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.nwdiag\\'" . graphviz-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.rackdiag\\'" . graphviz-mode))
 
  t))

(autoload-r! graphviz-dot-mode
	     (graphviz)
	     "graphviz-dot-mode"
	     t)

(rmode! graphviz-mode
	"Emacs mode to edit graphviz files"
	(graphviz)
	config-graphviz
	graphviz-dot-mode)

