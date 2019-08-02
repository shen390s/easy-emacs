;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded

;; Enable features
;;  features in global scope
(enable! global
	 (emacs-server
	  graphviz
	  ;;eldoc
	  ;;treemacs
	  ;;powerline
	  (smart-mode-line :activate
			   (:pre ((setq sml/no-confirm-load-theme t)
				  (setq sml/theme 'dark))))
	  ;;projectile
	  ;;color-theme-solarized
	  ;;color-theme-modern
	  load-custom
	  ;;zenburn-emacs
	  (sanityinc-tomorrow 'blue)
          ;;atom-one-dark
          ;;aanila
          ;;tango-dark
          ;;(color-theme 'tango-dark)
          ;;(color-theme  'tsdh-dark)
	  which-key
	  (ivy :activate
	       (:post ((setq ivy-use-virtual-buffers t)
	  	       (setq ivy-count-format "(%d/%d) "))))
	  ;;helm
	  undo-tree
	  evil
          show-paren-line
	  ;;boon
          ;;ergoemacs
	  yasnippet
	  magit
          forge
	  rust
	  clojure
          ess
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
	  fold-this
	  hlinum))

;; features in c/c++
(enable!  c-c++
	  (lsp-ui
	   ;;stickyfunc-enhance
	   ;;eglot
	   (cquery :activate
	   	   (:pre ((setq cquery-executable "/opt/tools/cquery/bin/cquery"))))
	   ggtags
	   (call-graph
	    :activate (:pre ((setq cg-initial-max-depth 3))))
	   ;;ccls
	   ;;clangd
	   (set-c-style :activate
			(:pre ((c-set-style "cc-mode"))))))

;; features in elisp scope
(enable! elisp
	 ())

;; features in python scope
(enable! python
	 ((lsp-python-ms :activate
			 (:pre ((setq lsp-python-ms-executable
				      "/opt/tools/mspyls/Microsoft.Python.LanguageServer"))))
	 ))

;; features in rust scope
(enable! rust
	 (rls
          ))

;; features in java scope
(enable! java
	 (lsp-java
	  ggtags))

(enable! graphviz
	 ())

(enable! markdown
	 (hlinum
	  vmd))

