(package! graphviz
	  "Emacs mode for the graphviz"
	  (graphviz-dot-mode :type git
			     :host github
			     :repo "ppareit/graphviz-dot-mode"))

(mode! graphviz-dot-mode
       "Emacs mode to edit graphviz"
       (graphviz)
       'graph-dot-mode)

(defun graphviz-config ()
  (progn
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
		 '("\\.rackdiag\\'" . graphviz-dot-mode))
    (attach! graphviz graphviz-dot-mode)
    t))

(scope! graphviz prog-lang graphviz-config)

(defun graphviz-dot-mode-load ()
  (require 'graphviz-dot-mode))

(add-hook (scope-function 'graphviz 'hook :before)
	  'graphviz-dot-mode-load)

(attach! graphviz graphviz-dot-mode)

