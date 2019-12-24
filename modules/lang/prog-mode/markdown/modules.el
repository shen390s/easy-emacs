(scope! markdown prog-lang)

(package! :name markdown-mode
	  :docstring "A major Emacs mode for edit markdown document"
	  :pkginfo markdown-mode)

(package! :name vmd-mode
	  :docstring "Snappy Markdown preview minor mode for emacs"
	  :pkginfo (vmd-mode :type git
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

(rmode! lang/markdown-mode
	"Emacs mode for markdown"
	(markdown-mode)
	config-markdown
	markdown-mode)

(autoload-r! gfm-mode
	     (markdown-mode)
	     "markdown-mode"
	     t)

(rmode! lang/gfm-mode
	"README.md"
	(markdown-mode)
	config-markdown
	gfm-mode)

(defun enable-vmd ()
  (vmd-mode))

(feature! vmd
	  "Snappy Markdown preview minor mode for emacs"
	  (vmd-mode)
	  nil
	  enable-vmd
	  nil)
