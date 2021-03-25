;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded
(require 'tramp)

(setq use-polymode t)
(setq byte-compile-debug t)

;; use long idle time for defer package install
(setq easy-emacs-idle-time (* 15 60))

(add-to-list 'cquery-enabled-path-list
	     "/Users/rshen/workspace")
(add-to-list 'cquery-enabled-path-list
             "/Users/rshen/sources")

;; Enable features
;;  features in global scope
(enable! global
	 ((settings java-home
		    "/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home"
		    use-polymode t)
	  smex
	  emacs-server
	  ;;treemacs
	  ;;powerline
	  (smart-mode-line :activate
			   (:pre ((setq sml/no-confirm-load-theme t)
				  (setq sml/theme 'dark))))
	  ;;projectile
	  ;;color-theme-solarized
	  ;;color-theme-modern
	  (load-custom 'rshen)
	  ;;zenburn-emacs
	  ;;(sanityinc-tomorrow 'blue)
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
	  (evil
	   ;; setup key binding in global scope
	   (kbd "b") 'counsel-switch-buffer
	   (kbd "e") 'counsel-find-file)
          show-paren-line
	  ;;boon
          ;;ergoemacs
	  yasnippet
	  magit
          (forge :activate
		 (:post ((add-to-list 'forge-alist
				    '("github.ibm.com"
				      "api.github.ibm.com"
				      "github.ibm.com"
				      forge-github-repository)))))
	  golden-ratio
	  icicles
	  emacs-quilt ;; to enable emacs and quilt integration
	  ;;dash
	  ;;devdocs
          ;;eaf
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
	  ;;fold-this
	  vimish-fold
	  ;;origami
	  ruler
          auto-fill
          hlinum))

;; features in c/c++
(enable!  c-c++
	  (lsp-ui
	   ;;stickyfunc-enhance
	   ;;eglot
	   (cquery :when (let ((b (buffer-file-name)))
			   (if (and b
				    (not (tramp-tramp-file-p b)))
			       (file-in-path-list b
						  cquery-enabled-path-list)
			     t))
		   :activate
	   	   (:pre ((setq cquery-executable "/opt/tools/cquery/bin/cquery"))))
	   ggtags
	   ;;tree-sitter
	   (call-graph
	    :activate (:pre ((setq cg-initial-max-depth 3))))
	   ;;ccls
	   ;;(ccls :activate
	   ;;      (:pre ((setq ccls-executable "/opt/tools/ccls/bin/ccls"))))
	   ;;clangd
	   (set-c-style :activate
			(:pre ((c-set-style "cc-mode"))))))

;; features in elisp scope
;;(enable! elisp
;;	 ())

;; features in python scope
(enable! python
	 ((lsp-python-ms :activate
			 (:pre ((setq lsp-python-ms-executable
				      "/opt/tools/mspyls/Microsoft.Python.LanguageServer"))))
	  ;;tree-sitter
	  ))

;; features in rust scope
(enable! rust
	 (rls
	  ;;tree-sitter
          ))

;;(enable! fish
;;	 ())

;; features in java scope
(enable! java
	 (lsp-java
	  ggtags
	  ;;tree-sitter
	  ))

;;(enable! graphviz
;;	 ())

(enable! markdown
	 (;;format-all
	  vmd))

(enable! poly-mode
	 (;;format-all
	  vmd
	  ))

;;(enable! plantuml ())

(enable! tex
	 (magic-latex
	  auctex))

(enable! go
         (lsp))

(enable! mermaid
         ())

(if use-polymode
    (enable! ascii-doctor
	     (livemarkup  poly-asciidoc-keybind))
  (enable! ascii-doctor
	   (livemarkup)))

(attach! graphviz
	 graphviz-mode)
(attach! rust
	 lang/rust-mode)
(attach! fish
         lang/fish-mode)
(attach! plantuml
	 lang/plantuml-mode)
(attach! elisp
	 emacs-lisp-mode
	 lisp-interaction-mode)
(attach! python
	 python-mode)
(attach! c-c++
	 c-mode
	 c++-mode)
(attach! java
	 java-mode)
(attach! clojure
	 lang/clojure-mode)

(if use-polymode
    (progn
      (attach! poly-mode
	       lang/markdown-mode
	       lang/gfm-mode
	       markdown-mode
	       gfm-mode)
      (attach! ascii-doctor
	       lang/poly-asciidoc-mode))
  (progn
    (attach! markdown
	     lang/markdown-mode
	     lang/gfm-mode
	     markdown-mode
	     gfm-mode)
    (attach! ascii-doctor
	     lang/adoc-mode)))

(attach! poly-mode
	 lang/poly-markdown-mode
	 lang/poly-R-mode
	 lang/poly-org-mode
	 lang/poly-asciidoc-mode)

(attach! ascii-doctor
	 adoc-mode)

(attach! racket
	 lang/racket-mode)
(attach! racket
	 lang/pollen-mode)

(attach! tex LaTeX-mode
	 latex-mode)

(attach! prog-lang
         makefile-mode
	 lang/yaml-mode
	 yaml-mode
	 makefile-bsdmake-mode)

(attach! shell-script
         shell-script-mode
         sh-mode)

(attach! go
         go-mode
         lang/go-mode)

(attach! mermaid
         mermaid-mode
         lang/mermaid-mode)
