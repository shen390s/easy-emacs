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

(package! :name poly-asciidoc
	  :docstring "Poly asciidoc mode"
	  :pkginfo (poly-asciidoc :type git
				  :host github
				  :repo "shen390s/poly-asciidoc"))
(defun reassoc-md-ext ()
  (unassoc-ext "\\.md\\'")
  (unassoc-ext "\\.markdown\\'")
  (add-to-list 'auto-mode-alist
	       '("\\.md\\'" . lang/poly-markdown-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.markdown\\'" . lang/poly-markdown-mode)))

(defun config-poly-markdown ()
  (DEBUG! "configuring poly-markdown mode ...")
  (and use-polymode
       (progn 
	 (reassoc-md-ext)
      	 (with-eval-after-load "markdown-mode"
	   (reassoc-md-ext))
	 t)))

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
  (unassoc-ext "\\.Rmd\\'")
  (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . lang/poly-R-mode)))

(defun config-poly-R ()
  (and use-polymode
       (progn
	 (reassoc-R-ext)
	 (with-eval-after-load "ess"
	   (reassoc-R-ext))
	 t)))

(autoload-r! poly-noweb+r-mode
	     (polymode poly-R ess)
	     "poly-R"
	     t)

(rmode! lang/poly-R-mode
	"Emacs mode for poly R"
	(polymode poly-R ess markdown-mode)
	config-poly-R
	poly-markdown+R-mode)

(defun hack-poly-org ()
       (unassoc-ext "\\.org\\'")
       (add-to-list 'auto-mode-alist '("\\.org\\'" . lang/poly-org-mode)))

(defun config-poly-org ()
  (and use-polymode
       (progn
	 (hack-poly-org)
	 (with-eval-after-load "org"
	   (hack-poly-org))
	 t)))

(autoload-r! poly-org-mode
	     (polymode poly-org)
	     "poly-org"
	     t)

(rmode! lang/poly-org-mode
	"Emacs mode for poly ORG"
	(polymode poly-org)
	config-poly-org
	poly-org-mode)

(defun reassoc-adoc-ext ()
  (unassoc-ext "\\.adoc\\'")
  (add-to-list 'auto-mode-alist
	       '("\\.adoc\\'" . lang/poly-asciidoc-mode)))

(defun config-poly-asciidoc ()
  (and use-polymode
       (progn
	 (reassoc-adoc-ext)
	 (with-eval-after-load "adoc-mode"
	   (reassoc-adoc-ext))
	 t)))

(autoload-r! poly-asciidoc-mode
	     (polymode poly-asciidoc-mode adoc-mode)
	     "poly-asciidoc"
	     t)

(rmode! lang/poly-asciidoc-mode
	"Emacs mode for poly asciidoc"
	(polymode poly-asciidoc-mode adoc-mode)
	config-poly-asciidoc
	poly-asciidoc-mode)
