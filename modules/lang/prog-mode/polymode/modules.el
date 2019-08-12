(scope! poly-mode
	prog-lang)

(package! polymode
	  "Extensible framework for multiple major modes"
	  polymode)

(package! poly-markdown
	  "polymode for markdown mode"
	  poly-markdown)

(defun config-poly-markdown ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.md$" . poly-markdown-mode))
    (mode! poly-mode poly-markdown-mode)
    t))

(defun activate-poly-markdown ()
  (require 'poly-markdown)
  (poly-markdown-mode 1))

(feature! poly-markdown
	  "polymode for markdown mode"
	  (polymode poly-markdown markdown-mode)
	  config-poly-markdown
	  nil ;;activate-poly-markdown
	  nil)
