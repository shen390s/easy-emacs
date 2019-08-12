(scope! poly-mode
	prog-lang)

(package! polymode
	  "Extensible framework for multiple major modes"
	  polymode)

(package! poly-markdown
	  "polymode for markdown mode"
	  poly-markdown)

(package! poly-org
	  "Poly org mode"
	  (poly-org :type git
		    :host github
		    :repo "emacsmirror/poly-org"))

(package! poly-R
	  "Poly R mode"
	  (poly-R :type git
		    :host github
		    :repo "emacsmirror/poly-R"))

(defun config-poly-markdown ()
  (progn
    (require 'poly-markdown)
    (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
    (mode! poly-mode poly-markdown-mode)
    t))

(feature! poly-markdown
	  "polymode for markdown mode"
	  (polymode poly-markdown markdown-mode)
	  config-poly-markdown
	  nil 
	  nil)

(defun config-poly-R ()
  (progn
;;    (add-to-list 'auto-mode-alist '("\\.r$" . poly-R-mode))
    (require 'poly-R)
    (mode! poly-mode poly-noweb+r-mode)
    t))

(feature! poly-R
	  "polymode for R"
	  (polymode poly-R ess)
	  config-poly-R
	  nil 
	  nil)

(defun config-poly-org ()
  (progn
    (require 'poly-org)
    (mode! poly-mode poly-org-mode)
    t))

(feature! poly-org
	  "Poly mode for org"
	  (polymode poly-org org-mode)
	  config-poly-org
	  nil
	  nil)
