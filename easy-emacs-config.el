;; Configuration for easy-emacs
;; this file will be loaded

;; Enable features
;;  features in global scope
(enable! global
	 (emacs-server
	  ;;eldoc
	  ;;treemacs
	  ;;powerline
	  (smart-mode-line :activate
			   (:pre ((setq sml/no-confirm-load-theme t)
				  (setq sml/theme 'dark))))
	  ;;projectile
	  ;;color-theme-solarized
	  ;;color-theme-modern
	  which-key
	  (ivy :activate
	       (:post ((setq ivy-use-virtual-buffers t)
		       (setq ivy-count-format "(%d/%d) "))))
	  undo-tree
	  evil
	  yasnippet
	  magit
	  rust
	  clojure
	  (markdown :activate
		    (:pre ((setq markdown-command "multimarkdown"))))
	  (plantuml :activate
		    (:pre ((setq plantuml-jar-path
				 "/Users/rshen/workenv/emacs/jars/plantuml.jar"))))
	  golden-ratio
	  (slime :activate
		 (:pre ((setq inferior-lisp-program "/opt/pkg/bin/sbcl")
			(setq slime-contribs '(slime-fancy)))))))

;; features in program languages scope
(enable! prog-lang
	 (smartparens
	  which-func
	  rainbow-delimiters
	  rainbow-identifiers
	  -flymake
	  hlinum))

;; features in c/c++
(enable!  c-c++
	  (lsp-ui
	   ;;stickyfunc-enhance
	   ;;eglot
	   cquery
	   ;; (cquery :activate (:pre ((setq cquery-executable "/usr/local/bin/cquery"))))
	   ;;ccls
	   (set-c-style :activate
			(:pre ((c-set-style "stroustrup"))))))

;; features in elisp scope

;; features in python scope
(enable! python
	 ((lsp-python-ms :activate
			 (:pre ((setq lsp-python-ms-executable
				      "/Users/rshen/bin/Microsoft.Python.LanguageServer"))))))

;; features in rust scope
(enable! rust
	 (rls))

;; features in java scope
(enable! java
	 (lsp-java))

(enable! markdown
	 (hlinum
	  vmd))

