(scope! poly-mode
	prog-lang)

(package! polymode
	  "Extensible framework for multiple major modes"
	  polymode)

(package! poly-markdown
	  "polymode for markdown mode"
	  poly-markdown)

(defun activate-poly-markdown ()
  (require 'poly-markdown)
  (poly-markdown-mode 1))

(feature! poly-markdown
	  "polymode for markdown mode"
	  (polymode poly-markdown markdown)
	  nil
	  activate-poly-markdown
	  nil)
