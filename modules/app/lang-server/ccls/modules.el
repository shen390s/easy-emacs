(package!
 emacs-ccls
 "Use ccls as c/c++ language server"
 (emacs-ccls :type git :host github :repo "MaskRay/emacs-ccls"))

(defun ccls-on ()
  (require 'ccls)
  (progn
    (if (feature-enabled 'yasnippet)
	(setq lsp-enable-snippet t)
      (setq lsp-enable-snippet nil))
    (let ((feature-actived-scope (feature-enabled 'lsp-ui)))
      (when (member current-scope feature-actived-scope)
	(add-hook 'lsp-mode-hook 'lsp-ui-mode)))
    (enable-lsp)))

(feature!
 ccls
 "Use ccls as c/c++ language server"
 (lsp-module emacs-ccls)
 nil
 ccls-on
 nil)
