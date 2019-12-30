(scope! poly-mode prog-lang)

(package! :name polymode
	  :docstring "Extensible framework for multiple major modes"
	  :pkginfo (polymode :type git
			     :host github
			     :repo "emacsmirror/polymode"))

(package! :name poly-markdown
	  :docstring "polymode for markdown mode"
	  :pkginfo (poly-markdown :type git
				  :host github
				  :repo "emacsmirror/poly-markdown"))

(package! :name poly-org
	  :docstring "Poly org mode"
	  :pkginfo (poly-org :type git
			     :host github
			     :repo "emacsmirror/poly-org"))

(package! :name poly-R
	  :docstring "Poly R mode"
	  :pkginfo (poly-R :type git
			   :host github
			   :repo "emacsmirror/poly-R"))

(defun reassoc-md-ext ()
  (unassoc-ext "\\.md\\'")
  (unassoc-ext "\\.markdown\\'")
  (add-to-list 'auto-mode-alist
	       '("\\.md\\'" . lang/poly-markdown-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.markdown\\'" . lang/poly-markdown-mode)))

(defun config-poly-markdown ()
  (progn
    (INFO! "configuring poly-markdown mode ...")
    (if use-polymode
      (progn 
	(reassoc-md-ext)
      	(with-eval-after-load "markdown-mode"
		(reassoc-md-ext))
	t)
    nil)))

(autoload-r! poly-markdown-mode
	     (polymode poly-markdown markdown-mode)
	     "poly-markdown"
	     t)

(rmode! lang/poly-markdown-mode
	"Emacs mode for poly markdown"
	(polymode poly-markdown markdown-mode)
	config-poly-markdown
	poly-markdown-mode)

(defun reassoc-R-ext ()
  (unassoc-ext "\\.r\\'")
  (add-to-list 'auto-mode-alist '("\\.r\\'" . lang/poly-R-mode)))

(defun config-poly-R ()
  (progn
    (if use-polymode
	(progn
	  (reassoc-R-ext)
	  t)
      nil)))

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

