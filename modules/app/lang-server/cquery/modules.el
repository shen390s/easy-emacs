(package!
 emacs-cquery
 "Use cquery as c/c++ language server"
 (emacs-cquery :type git :host github :repo "cquery-project/emacs-cquery"))

(defun cquery-on ()
  (require 'cquery)
  (progn
    (if (feature-enabled 'yasnippet)
	(setq lsp-enable-snippet t)
      (setq lsp-enable-snippet nil))
    (let ((feature-actived-scope (feature-enabled 'lsp-ui)))
      (when (member current-scope feature-actived-scope)
	(add-hook 'lsp-mode-hook 'lsp-ui-mode)))
    (enable-lsp)))

(feature!
 cquery
 "Use cquery as c/c++ language server"
 (lsp-module emacs-cquery)
 nil
 cquery-on
 nil)
