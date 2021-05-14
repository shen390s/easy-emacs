
(package-ex!  polymode
	      "Extensible framework for multiple major modes"
	      (polymode :type git
			:host github
			:repo "emacsmirror/polymode"))

(package-ex!  poly-markdown
	      "polymode for markdown mode"
	      (poly-markdown :type git
			     :host github
			     :repo "emacsmirror/poly-markdown"))

(package-ex!  poly-org
	      "Poly org mode"
	      (poly-org :type git
			:host github
			:repo "emacsmirror/poly-org"))

(package-ex!  poly-R
	      "Poly R mode"
	      (poly-R :type git
		      :host github
		      :repo "emacsmirror/poly-R"))

(package-ex!  poly-asciidoc
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
  (pcase phase
    (:check
     (after-activate! evil
		      (progn
			(DEBUG! "config-poly-asciidoc-keybind")
			(require 'evil-leader)
			(evil-leader/set-key-for-mode 'poly-asciidoc-mode
			  (kbd "mc") 'poly-asciidoc-compile
			  (kbd "mv") 'poly-asciidoc-view
			  (kbd "mf") 'poly-asciidoc-set-output-format))))
    (_ t)))

(feature-ex! poly-asciidoc-keybind
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
    (:pre-check
     (with-eval-after-load "markdown-mode"
       (DEBUG! "fix file assoc after markdown-mode.el loaded")
       (reassoc-ext 'markdown-mode 'poly-markdown-mode)
       (DEBUG! "fix file assoc auto-mode-alist %s"
	       auto-mode-alist)))
    (_ t)))

(feature-ex! fix-poly-markdown
	     "fix ext assoc of poly-markdown"
	     nil
	     config-fix-poly-markdown
	     nil
	     nil)

(add-auto-features "poly-markdown-mode" 'fix-poly-markdown)
