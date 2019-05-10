(package!
 lsp-module
 "Emacs lisp client to language server"
 lsp-mode)

(package!
 lsp-ui
 "This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses."
 (lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui"))

(defun lsp-setup-keybind ()
  (when (feature-enabled 'evil)
    (require 'evil)
    (define-key evil-normal-state-local-map "gd" 'lsp-find-definition)
    (define-key evil-normal-state-local-map "gr" 'lsp-find-references)
    (define-key evil-normal-state-local-map "gi" 'lsp-find-implementation)
    (define-key evil-normal-state-local-map "gt" 'lsp-find-type-definition)))

(defun enable-lsp ()
  (progn
    (lsp)
    (lsp-setup-keybind)))

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
