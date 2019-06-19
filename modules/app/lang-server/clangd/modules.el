(defun clangd-pkgs ()
  (let ((pkgs (cond
	       ((eq (lang-server-client) 'eglot) '(eglot))
	       (t '(lsp-module)))))
    pkgs))

(defun config-lang-client-clangd ()
    (if (eq (lang-server-client) 'eglot)
	(progn
	  (with-eval-after-load "eglot"
	    (setq eglot-auto-display-help-buffer t)
	    (add-to-list 'eglot-server-programs
			 '((c++-mode c-mode) . "clangd"))))
      t))

(defun config-clangd ()
  (config-lang-client-clangd))

(defun enable-clangd ()
  (if (feature-enabled 'yasnippet)
      (setq lsp-enable-snippet t)
    (setq lsp-enable-snippet nil))
  (let ((feature-actived-scope (feature-enabled 'lsp-ui)))
    (when (member current-scope feature-actived-scope)
      (add-hook 'lsp-mode-hook 'lsp-ui-mode)))
  (enable-lsp))

(feature! clangd
	  "use clangd as c/c++ language server"
	  clangd-pkgs
	  config-clangd
	  enable-clangd
	  nil)
