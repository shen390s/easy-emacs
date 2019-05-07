(package!
 lsp-module
 "Emacs lisp client to language server"
 lsp-mode)

(package!
 lsp-ui
 "This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses."
 (lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui"))

(feature!
 lsp
 "Enable lsp mode"
 (lsp-module)
 nil
 nil 
 nil)

(feature!
 lsp-ui
 "This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses."
 (lsp-ui)
 nil
 nil
 nil)
