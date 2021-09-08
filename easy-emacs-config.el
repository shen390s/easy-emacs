;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded

(easy! :init
       (setq a 10)
       (setq b 20)
       (setq c-eldoc-includes
	     "-I/usr/include -I/usr/local/include -I. -I..")
       (setq cg-initial-max-depth 3)
       :core
       (best_git_mirrors)
       :modes
       (c :suffix .c .cc .cpp .c++ .h .hpp
	  :features +eldoc +ggtags +google-c-style +call-graph
	  :keybinds
	  (kbd "mcg") 'call-graph)
       (emacs-lisp :suffix .el .el.gz
		   :features -parinfer -lsp)
       (lisp :suffix .cl .lisp
	     :features -parinfer)
       (mermaid :suffix .mmd
		:keybinds
		(kbd "mc") 'mermaid-compile
		(kbd "mv") 'mermaid-view)
       (plantuml :suffix .plantuml)
       (poly-markdown :suffix .md .markdown
		      :features +vmd)
       (poly-noweb+r :suffix .Rmd)
       (poly-org :suffix .org
		 :features +livemarkup)
       (poly-asciidoc :suffix .adoc
		      :features +livemarkup
		      :keybinds
		       (kbd "mc") 'poly-asciidoc-compile
		       (kbd "mv") 'poly-asciidoc-view
		       (kbd "mf") 'poly-asciidoc-set-output-format)
       (fish :suffix .fish)
       (tex :suffix .tex .latex .sty
	    :features +auctex +magic-latex)
       (python :suffix .py
	       :features +eglot)
       (rust :suffix .rs
	     :features +rls)
       (rst :suffix .rst .rest)
       (diff :suffix .diff)
       (prog :features +hlinum +ruler +rainbow-delimiters
	     +rainbow-identifiers +smartparens 
	     +iedit +lsp -flymake
	     ;;+eglot
	     :keybinds
	     (kbd "mjfb") 'beginning-of-defun
	     (kbd "mjfe") 'end-of-defun
	     (kbd "mgd") 'ggtags-find-definition
	     (kbd "mgr") 'ggtags-find-reference
	     (kbd "mgtr") 'ggtags-find-tag-regexp
	     (kbd "mgos") 'ggtags-find-other-symbol
	     (kbd "mgf") 'ggtags-find-file
	     (kbd "mld") #'lsp-find-definition
	     (kbd "mlr") #'lsp-find-references
	     (kbd "mli") #'lsp-find-implemention
	     (kbd "mlt") #'lsp-find-type-definition
	     )
       (call-graph :keybinds
		   (kbd "m+") 'cg-expand
		   (kbd "m-") 'cg-collapse
		   (kbd "mo") 'cg-goto-file-at-point
		   (kbd "mg") 'cg-at-point)
       :ui
       (evil
	:after-activate (progn
			  (evil-leader/set-leader "<SPC>")
			  (evil-leader/set-key
			    (kbd "bs") 'counsel-switch-buffer
			    (kbd "f") 'counsel-find-file
			    (kbd "wm") 'delete-other-windows
			    (kbd "bd") 'kill-buffer
			    (kbd "tc") 'transpose-chars
			    (kbd "tw") 'transpose-words
			    (kbd "tc") 'transpose-regions
			    (kbd "tp") 'transpose-paragraphs
			    (kbd "tse") 'transpose-sexps
			    (kbd "tst") 'transpose-sentences)
			  ;; for the problem of evil-leader-mode
			  ;; has not been enabled for some special
			  ;; buffer
			  (cl-loop for b in (buffer-list)
				   do (with-current-buffer b
					(evil-leader-mode 1)))))
       (smart-mode-line)
       (load-custom :theme rshen)
       (smex)
       (icicles)
       (powerline)
       :completion ivy 
       :editor
       (undo-tree)
       (yasnippet)
       (evil-surround)
       ;;(-eldoc)
       :app
       (emacs-server)
       (which-key)
       (magit)
       (emacs-quilt)
       (origami)
       (treemacs +evil +magit)
       (noccur)
       (slime +slime-fancy
	      :before-activate (progn
				 (DEBUG! "configure slime...")
				 (setq inferior-lisp-program "/Users/rshen/.nix-profile/bin/sbcl")
				 (setq slime-contribs '(slime-fancy)))))

;;(debug-on-entry 'evil-leader/set-key-for-mode)
