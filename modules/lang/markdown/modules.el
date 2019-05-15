(scope! markdown (markdown-mode-hook) nil)

(package!
 markdown-mode
 "A major Emacs mode for edit markdown document"
 (markdown-mode :type git :host github :repo "jrblevin/markdown-mode"))

(defun config-markdown ()
  (require 'markdown-mode)
  (add-to-list 'auto-mode-alist
	       '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist
	       '("README\\.md\\'" . gfm-mode))
  t)

(feature!
 markdowm
 "Support to edit markdown document"
 (markdown-mode)
 config-markdown
 nil
 nil)

