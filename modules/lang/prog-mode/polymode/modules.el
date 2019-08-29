(scope! poly-mode
	prog-lang)

(package! polymode
	  "Extensible framework for multiple major modes"
	  (polymode :type git
		    :host github
		    :repo "emacsmirror/polymode"))

(package! poly-markdown
	  "polymode for markdown mode"
	  (poly-markdown :type git
			 :host github
			 :repo "emacsmirror/poly-markdown"))

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
    (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
    t))

(defun activate-poly-markdown ()
  (require 'poly-markdown-mode)
  (poly-markdown-mode)
  (attach! poly-mode poly-markdown-mode markdown-mode))

(mode! lang/poly-markdown-mode
       "Emacs mode for poly markdown"
       (polymode polydown-mode markdown-mode)
       config-poly-markdown
       activate-poly-markdown)

(feature! poly-markdown
	  "polymode for markdown mode"
	  (polymode poly-markdown markdown-mode)
	  config-poly-markdown
	  nil 
	  nil)

(defun config-poly-R ()
  (progn
;;    (add-to-list 'auto-mode-alist '("\\.r$" . poly-R-mode))
    (attach! poly-mode poly-noweb+r-mode)
    t))

(feature! poly-R
	  "polymode for R"
	  (polymode poly-R ess)
	  config-poly-R
	  nil 
	  nil)

(defun config-poly-org ()
  (progn
    (attach! poly-mode poly-org-mode)
    t))

(feature! poly-org
	  "Poly mode for org"
	  (polymode poly-org)
	  config-poly-org
	  nil
	  nil)
