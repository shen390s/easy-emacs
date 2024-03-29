;; -*- lexical-binding: t -*-

(package! mermaid-mode
	  "A major Emacs mode for edit mermaid document"
	  (mermaid-mode :type git
			:host github
			:repo "shen390s/mermaid-mode"))

(defun config-mermaid (scope &optional phase options)
  (DEBUG! "config-mermaid scope %s phase %s options %s"
	  scope phase options)
  t)

(autoload-r! mermaid-mode
	     (mermaid-mode)
	     "mermaid"
	     t)

(add-auto-features "mermaid-mode" 'mermaid)

(feature! mermaid
	  "Feature for mermaid-mode"
	  (mermaid-mode)
	  config-mermaid
	  nil
	  nil)

(add-to-list 'auto-mode-alist
	     '("\\.mmd\\'" . mermaid-mode))
