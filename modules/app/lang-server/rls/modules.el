(defun rls-on ()
  (let ((feature-actived-scope (feature-enabled 'lsp-ui)))
    (when (member current-scope feature-actived-scope)
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)))
  (lsp))

(feature!
 rls
 "Rust language server"
 (lsp-module)
 nil
 rls-on
 nil)
