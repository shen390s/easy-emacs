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

(defun activate-markdown ()
  (require 'markdown-mode)
  (attach! markdown markdown-mode)
  (markdown-mode))

(mode! lang/markdown-mode
       "Emacs mode for markdown"
       (markdown-mode)
       config-markdown
       activate-markdown)

(defun activate-gfm ()
  (require 'markdown-mode)
  (attach! markdown gfm-mode)
  (gfm-mode))

(mode! lang/gfm-mode
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
