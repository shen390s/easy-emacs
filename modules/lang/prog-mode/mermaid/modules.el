(scope! mermaid prog-lang)

(package! :name mermaid-mode
	  :docstring "A major Emacs mode for edit mermaid document"
	  :pkginfo (mermaid-mode :type git
				 :host github
				 :repo "shen390s/mermaid-mode"))

(defun config-mermaid ()
  (DEBUG! "configuring mermaid mode...")
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.mmd\\'" . lang/mermaid-mode))
    t))

(autoload-r! mermaid-mode
	     (mermaid-mode)
	     "mermaid-mode"
	     t)

(rmode! lang/mermaid-mode
	"Emacs mode for mermaid"
	(mermaid-mode)
	config-mermaid
	mermaid-mode)

