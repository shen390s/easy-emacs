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
    (INFO! "configuring poly-markdown mode ...")
    t))

(autoload-r! poly-markdown-mode
	     (polymode poly-markdown markdown-mode)
	     "poly-markdown"
	     t)

(rmode! lang/poly-markdown-mode
	"Emacs mode for poly markdown"
	(polymode poly-markdown markdown-mode)
	config-poly-markdown
	poly-markdown-mode)

(defun config-poly-R ()
  (progn
    (rassq-delete-all 'r-mode auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.r\\'" . lang/poly-R-mode))
    t))

(autoload-r! poly-noweb+r-mode
	     (polymode poly-R ess)
	     "poly-R"
	     t)

(rmode! lang/poly-R-mode
	"Emacs mode for poly R"
	(polymode poly-R ess)
	config-poly-R
	poly-noweb+r-mode)

(defun config-poly-org ()
  (progn
    (rassq-delete-all 'org-mode auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.org\\'" . lang/poly-org-mode))
    t))

(autoload-r! poly-org-mode
	     (polymode poly-org)
	     "poly-org"
	     t)

(rmode! lang/poly-org-mode
	"Emacs mode for poly ORG"
	(polymode poly-org)
	config-poly-org
	poly-org-mode)

