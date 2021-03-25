(defun rls-on ()
  (when (feature-in-scope 'lsp-ui current-scope)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  (lsp))

(feature! rls
	  "Rust language server"
	  (lsp-module)
	  nil
	  rls-on
	  nil)
