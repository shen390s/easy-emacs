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

(defun hack-markdown ()
    (setq auto-mode-alist (rassq-delete-all 'markdown-mode auto-mode-alist))
    (add-to-list 'auto-mode-alist '("\\.md$" . lang/poly-markdown-mode)))

(defun config-poly-markdown ()
  (progn
    (INFO! "configuring poly-markdown mode ...")
    (hack-markdown)
    (with-eval-after-load "markdown-mode"
      (hack-markdown))
    t))

(defun activate-poly-markdown ()
  (require 'poly-markdown)
  (poly-markdown-mode)
  (attach! poly-mode poly-markdown-mode markdown-mode))

(mode! lang/poly-markdown-mode
       "Emacs mode for poly markdown"
       (polymode poly-markdown markdown-mode)
       config-poly-markdown
       activate-poly-markdown)

(defun config-poly-R ()
  (progn
    (rassq-delete-all 'r-mode auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.r\\'" . lang/poly-R-mode))
    t))

(defun activate-poly-R ()
  (require 'poly-R)
  (poly-noweb+r-mode)
  (attach! poly-mode poly-noweb+r-mode))

(mode! lang/poly-R-mode
       "Emacs mode for poly R"
       (polymode poly-R ess)
       config-poly-R
       activate-poly-R)

(defun config-poly-org ()
  (progn
    (rassq-delete-all 'org-mode auto-mode-alist)
    (add-to-list 'auto-mode-alist '("\\.org\\'" . lang/poly-org-mode))
    t))

(defun activate-poly-org ()
  (require 'poly-org)
  (poly-org-mode)
  (attach! poly-mode poly-org-mode))

(mode! lang/poly-org-mode
       "Emacs mode for poly ORG"
       (polymode poly-org)
       config-poly-org
       activate-poly-org)

