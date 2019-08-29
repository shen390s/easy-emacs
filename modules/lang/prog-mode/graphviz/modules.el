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

(defun activate-graphviz (&rest args)
  (require 'graphviz-dot-mode)
  (graphviz-dot-mode))

(mode! graphviz-mode
       "Emacs mode to edit graphviz files"
       (graphviz)
       config-graphviz
       activate-graphviz)

(defun graphviz-enable ()
  t)

(feature! graphviz
	  "Emacs mode for graphviz"
	  (graphviz)
	  graphviz-config
	  graphviz-enable
	  nil)

