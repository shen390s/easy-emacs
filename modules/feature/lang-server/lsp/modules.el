(package!
 lsp-module
 "Emacs lisp client to language server"
 lsp-mode)

(feature!
 lsp
 "Enable lsp mode"
 (lsp-module)
 nil
 nil ;; FIXME: add enable function later
 nil)
