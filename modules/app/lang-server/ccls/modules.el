(package!
 emacs-ccls
 "Use ccls as c/c++ language server"
 (emacs-ccls :type git :host github :repo "MaskRay/emacs-ccls"))

(defun config-ccls ()
  (if (conflict-features current-scope 'cquery)
      (progn
	(message "C/C++ language server `cquery' has already been enabled in scope %s `ccls' language server will be disabled"
		 current-scope)
	  nil)
    t))

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
 config-ccls
 ccls-on
 nil)
