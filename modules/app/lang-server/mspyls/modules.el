(package! :name lsp-python-ms
	  :docstring "emacs lsp-mode client for Microsoft's python language server "
	  :pkginfo (lsp-python-ms :type git :host github :repo "emacs-lsp/lsp-python-ms"))

(defun enable-lsp-python-ms ()
  (let ((feature-actived-scope (feature-enabled 'lsp-ui)))
      (when (member current-scope feature-actived-scope)
	(add-hook 'lsp-mode-hook 'lsp-ui-mode)))
  (require 'lsp-python-ms)
  (lsp))

(feature! lsp-python-ms
 "Emacs lsp-mode client for Microsoft python language server"
 (lsp-module lsp-python-ms)
 nil
 enable-lsp-python-ms
 nil)
