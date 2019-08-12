(scope! poly-mode
	prog-lang)

(package! polymode
	  "Extensible framework for multiple major modes"
	  polymode)

(package! poly-markdown
	  "polymode for markdown mode"
	  poly-markdown)

(defun config-poly-markdown ()
  t)

(defun activate-poly-markdown ()
  (require 'poly-markdown)
  (poly-markdown-mode 1))

(feature! poly-markdown
	  "polymode for markdown mode"
	  (polymode poly-markdown markdown)
	  config-poly-markdown
	  activate-poly-markdown
	  nil)
