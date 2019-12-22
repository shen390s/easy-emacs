(scope! markdown prog-lang)

(package! markdown-mode
	  "A major Emacs mode for edit markdown document"
	  markdown-mode)

(package! vmd-mode
	  "Snappy Markdown preview minor mode for emacs"
	  (vmd-mode :type git
		    :host github
		    :repo "blak3mill3r/vmd-mode"))

(defun hack-markdown ()
  (setq auto-mode-alist
	(rassq-delete-all 'markdown-mode auto-mode-alist))
  (setq auto-mode-alist
	(rassq-delete-all 'gfm-mode auto-mode-alist)))

(defun config-markdown ()
  (hack-markdown)
  (add-to-list 'auto-mode-alist
	       '("\\.md\\'" . lang/markdown-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.markdown\\'" . lang/markdown-mode))
  (add-to-list 'auto-mode-alist
	       '("README\\.md\\'" . lang/gfm-mode))
  t)

(autoload-r! markdown-mode
	     (markdown-mode)
	     "markdown-mode"
	     t)

(defun activate-markdown ()
  (attach! markdown markdown-mode)
  (markdown-mode))

(rmode! lang/markdown-mode
	"Emacs mode for markdown"
	(markdown-mode)
	config-markdown
	activate-markdown)

(autoload-r! gfm-mode
	     (markdown-mode)
	     "markdown-mode"
	     t)

(defun activate-gfm ()
  (attach! markdown gfm-mode)
  (gfm-mode))

(rmode! lang/gfm-mode
	"README.md"
	(markdown-mode)
	config-markdown
	activate-gfm)

(defun enable-vmd ()
  (vmd-mode))

(feature! vmd
	  "Snappy Markdown preview minor mode for emacs"
	  (vmd-mode)
	  nil
	  enable-vmd
	  nil)
