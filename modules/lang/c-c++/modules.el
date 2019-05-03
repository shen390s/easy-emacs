;;; modules.el --- description

(scope! c-c++ (c-mode-hook c++-mode-hook) nil)

(package!
 lsp-module
 "Emacs lisp client to language server"
 (:github "emacs-lsp/lsp-mode")
 ((lsp
   "Enable lsp mode"
   nil
   nil
   nil)))


