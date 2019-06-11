(scope! graphviz prog-lang graphviz-dot-mode)

(package! graphviz
	  "Emacs mode for the graphviz"
	  (graphviz-dot-mode :type git
			     :host github
			     :repo "ppareit/graphviz-dot-mode"))

(defun graphviz-config ()
  (add-to-list 'auto-mode-alist
	       '("\\.dot\\'" . graphviz-dot-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.diag\\'" . graphviz-dot-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.gv\\'" . graphviz-dot-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.blockdiag\\'" . graphviz-dot-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.nwdiag\\'" . graphviz-dot-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.rackdiag\\'" . graphviz-dot-mode)))

(defun graphviz-enable ()
  t)

(feature! graphviz
	  "Emacs mode for graphviz"
	  (graphviz)
	  graphviz-config
	  graphviz-enable
	  nil)

