;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded

(easy! :init
       (setq a 10)
       (setq b 20)
       (setq c-eldoc-includes
	     "-I/usr/include -I/usr/local/include -I. -I..")
       (setq cg-initial-max-depth 3)
       ;; disable confirm of loading theme
       (setq custom-safe-themes t)
       
       :core
       (best_git_mirrors)

       :modes
       (c :inherit prog ggtags
	  :attach c-mode 
	  :features +eldoc +google-c-style +call-graph
	  :keybinds
	  (kbd "mcg") 'call-graph)
       (emacs-lisp :inherit prog
		   :attach emacs-lisp-mode lisp-interactive-mode
		   :features -parinfer -lsp)
       (lisp :inherit prog
	     :attach lisp-mode
	     :features -parinfer)
       (mermaid :inherit basic
		:attach mermaid-mode
		:keybinds
		(kbd "mc") 'mermaid-compile
		(kbd "mv") 'mermaid-view)
       (plantuml :inherit basic
		 :attach plantuml-mode)
       (poly-markdown :inherit basic
		      :attach markdown-mode poly-markdown-mode
		      :features +vmd)
       (poly-noweb+r :inherit basic
		     :attach poly-noweb+r-mode)
       (poly-org :inherit basic
		 :attach org-mode
		 :features +livemarkup)
       (poly-asciidoc :inherit basic
		      :attach poly-asciidoc-mode
		      :features +livemarkup
		      :keybinds
		       (kbd "mc") 'poly-asciidoc-compile
		       (kbd "mv") 'poly-asciidoc-view
		       (kbd "mf") 'poly-asciidoc-set-output-format)
       (fish :inherit prog
	     :attach fish-mode)
       (tex :inherit prog
	    :attach tex-mode
	    :features +auctex +magic-latex)
       (python :inherit prog lsp
	       :attach python-mode)
       (rust :inherit prog lsp
	     :attach rust-mode
	     :features +rls)
       (rst :inherit basic
	    :attach rst-mode)
       (diff :inherit basic
	     :attach diff-mode)
       (basic :features +hlinum +ruler +smartparens +iedit)
       (prog :inherit basic
	     :features +rainbow-delimiters +rainbow-identifiers 
	     +iedit -flymake
	     ;;+eglot
	     :keybinds
	     (kbd "mjfb") 'beginning-of-defun
	     (kbd "mjfe") 'end-of-defun)
       (ggtags :features +ggtags
	       :keybinds
	       (kbd "mgf") 'ggtags-find-file
	       (kbd "mgd") 'ggtags-find-definition
	       (kbd "mgr") 'ggtags-find-reference
	       (kbd "mgtr") 'ggtags-find-tag-regexp
	       (kbd "mgos") 'ggtags-find-other-symbol)
       (lsp :features +lsp
	    :keybinds
	    (kbd "mld") #'lsp-find-definition
	    (kbd "mlr") #'lsp-find-references
	    (kbd "mli") #'lsp-find-implemention
	    (kbd "mlt") #'lsp-find-type-definition)
       (call-graph :attach call-graph-mode
		   :keybinds
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
       ;;(powerline)
       (telephone-line
	:before-activate (progn
			   (require 'telephone-line)
			   ;; (setq telephone-line-primary-left-separator 'telephone-line-abs-left
			   ;; 	 telephone-line-primary-right-separator 'telephone-line-gradient
			   ;; 	 telephone-line-secondary-left-separator 'telephone-line-nil
			   ;; 	 telephone-line-secondary-right-separator 'telephone-line-nil)
			   
			   (defface my-red '((t (:foreground "white" :background "red"))) "")
			   (defface my-orangered '((t (:foreground "white" :background "orange red"))) "")
			   (defface my-orange '((t (:foreground "dim grey" :background "orange"))) "")
			   (defface my-gold '((t (:foreground "dim grey" :background "gold"))) "")
			   (defface my-yellow '((t (:foreground "dim grey" :background "yellow"))) "")
			   (defface my-chartreuse '((t (:foreground "dim grey" :background "chartreuse"))) "")
			   (defface my-green '((t (:foreground "dim grey" :background "green"))) "")
			   (defface my-sgreen '((t (:foreground "dim grey" :background "spring green"))) "")
			   (defface my-cyan '((t (:foreground "dim grey" :background "cyan"))) "")
			   (defface my-blue '((t (:foreground "white" :background "blue"))) "")
			   (defface my-dmagenta '((t (:foreground "white" :background "dark magenta"))) "")

			   (setq telephone-line-faces
				 '((red . (my-red . my-red))
				   (ored . (my-orangered . my-orangered))
				   (orange . (my-orange . my-orange))
				   (gold . (my-gold . my-gold))
				   (yellow . (my-yellow . my-yellow))
				   (chartreuse . (my-chartreuse . my-chartreuse))
				   (green . (my-green . my-green))
				   (sgreen . (my-sgreen . my-sgreen))
				   (cyan . (my-cyan . my-cyan))
				   (blue . (my-blue . my-blue))
				   (dmagenta . (my-dmagenta . my-dmagenta))
				   (evil . telephone-line-evil-face)
				   (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
				   (nil . (mode-line . mode-line-inactive))))

			   ;; (setq telephone-line-lhs
			   ;; 	 '((evil . (telephone-line-evil-tag-segment))
			   ;; 	   (accent . (telephone-line-vc-segment
			   ;; 		      telephone-line-process-segment))
			   ;; 	   ;; (ored . (s2))
			   ;; 	   ;; (orange . (s3))
			   ;; 	   ;; (gold . (s4))
			   ;; 	   ;; (yellow . (s5))
			   ;; 	   ;; (chartreuse . (s6))
			   ;; 	   ;; (green . (s7))
			   ;; 	   ;; (sgreen . (s8))
			   ;; 	   ;; (cyan . (s9))
			   ;; 	   ;; (blue . (s10))
			   ;; 	   ;; (dmagenta . (s11))
			   ;; 	   (nil    . (telephone-line-minor-mode-segment
			   ;; 		      telephone-line-buffer-segment))))

			   (setq telephone-line-lhs
				 '((red   . (telephone-line-evil-tag-segment))
				   (ored . (telephone-line-vc-segment))
				   ;;(orange .  (telephone-line-erc-modified-channels-segment))
				   (gold .  (telephone-line-process-segment))
				   (green    . (telephone-line-minor-mode-segment))
				   (nil .   (telephone-line-buffer-segment))
				   ;;(yellow . (mode-line-mule-info))
				   ))

			   (setq telephone-line-rhs
				 '((blue    . (telephone-line-misc-info-segment))
				   (orange . (telephone-line-major-mode-segment))
				   (dmagenta   . (telephone-line-airline-position-segment))))))
       
       :completion
       (ivy)
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
