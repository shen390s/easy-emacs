;; -*- lexical-binding: t -*-

(package!  polymode
	      "Extensible framework for multiple major modes"
	      (polymode :type git
			:host github
			:repo "emacsmirror/polymode"))

(package!  poly-markdown
	      "polymode for markdown mode"
	      (poly-markdown :type git
			     :host github
			     :repo "emacsmirror/poly-markdown"))

(package!  poly-org
	      "Poly org mode"
	      (poly-org :type git
			:host github
			:repo "emacsmirror/poly-org"))

(package!  poly-R
	      "Poly R mode"
	      (poly-R :type git
		      :host github
		      :repo "emacsmirror/poly-R"))

(package!  poly-asciidoc
	      "Poly asciidoc mode"
	      (poly-asciidoc :type git
			     :host github
			     :repo "shen390s/poly-asciidoc"))

(autoload-r! poly-markdown-mode
	     (polymode poly-markdown markdown-mode)
	     "poly-markdown"
	     t)

(autoload-r! poly-noweb+r-mode
	     (polymode poly-R ess)
	     "poly-R"
	     t)

(autoload-r! poly-org-mode
	     (polymode poly-org)
	     "poly-org"
	     t)

(defun config-poly-asciidoc-keybind (scope &optional phase options)
  t)

(feature! poly-asciidoc-keybind
	     "Bind keys for poly-asciidoc mode"
	     nil
	     config-poly-asciidoc-keybind
	     nil
	     nil)

(autoload-r! poly-asciidoc-mode
	     (polymode poly-asciidoc adoc-mode)
	     "poly-asciidoc"
	     t)

(defun config-fix-poly-markdown (scope &optional phase options)
  (pcase phase
    ('before
     (progn
       (with-eval-after-load "markdown-mode"
	 (DEBUG! "fix file assoc after markdown-mode.el loaded")
	 (reassoc-ext 'markdown-mode 'poly-markdown-mode)
	 (DEBUG! "fix file assoc auto-mode-alist %s"
		 auto-mode-alist))))
    (_ t)))

(feature! fix-poly-markdown
	     "fix ext assoc of poly-markdown"
	     nil
	     config-fix-poly-markdown
	     nil
	     nil)

(add-auto-features "poly-markdown-mode"
		   'poly-asciidoc-keybind
		   'fix-poly-markdown)
