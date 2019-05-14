(package!
 emacs-cquery
 "Use cquery as c/c++ language server"
 (emacs-cquery :type git :host github :repo "cquery-project/emacs-cquery"))

(defun config-cquery ()
  (if (conflict-features current-scope 'ccls)
      (progn
	(message "C/C++ language server `ccls' has already been enabled in scope %s `cquery' language server will be disabled"
		 current-scope)
	nil)
    t))

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
 config-cquery
 cquery-on
 nil)
