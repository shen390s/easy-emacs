(package! emacs-ccls
	  "Use ccls as c/c++ language server"
	  (emacs-ccls :type git
		      :host github
		      :repo "MaskRay/emacs-ccls"))

(defvar ccls-program nil
  "executable binary of ccls")

(defun config-lang-client-ccls ()
  (when (eq (lang-server-client) 'eglot)
    (with-eval-after-load "eglot"
      (setq eglot-auto-display-help-buffer t)
      (add-to-list 'eglot-server-programs
		   '((c++-mode c-mode) . ("ccls"))))))

(defun config-ccls ()
  (if (conflict-features current-scope 'cquery)
      (progn
	(INFO! "C/C++ language server `cquery' has already been enabled in scope %s `ccls' language server will be disabled"
	       current-scope)
	  nil)
    (progn
      (config-lang-client-ccls)
      t)))

(defun ccls/lsp-on ()
  (progn
    (if (feature-enabled 'yasnippet)
	(setq lsp-enable-snippet t)
      (setq lsp-enable-snippet nil))
    (let ((feature-actived-scope (feature-enabled 'lsp-ui)))
      (when (member current-scope feature-actived-scope)
	(add-hook 'lsp-mode-hook 'lsp-ui-mode)))
    (enable-lsp)))

(defun ccls/eglot-on ()
  (require 'eglot)
  (enable-eglot))

(defun ccls-on ()
  (require 'ccls)
  ;; customize ccls
  (when ccls-program
    (setq ccls-executable ccls-program))

  (cond
   ((eq (lang-server-client) 'eglot) (ccls/eglot-on))
   (t (ccls/lsp-on))))

(defun ccls-pkgs ()
  (let ((pkgs (cond
	       ((eq (lang-server-client) 'eglot) '(eglot emacs-ccls))
	       (t '(lsp-module emacs-ccls)))))
    pkgs))

(feature! ccls
	  "Use ccls as c/c++ language server"
	  ccls-pkgs
	  config-ccls
	  ccls-on
	  nil)
