(package! :name emacs-cquery
	  :docstring "Use cquery as c/c++ language server"
	  :pkginfo (emacs-cquery :type git
				 :host github
				 :repo "cquery-project/emacs-cquery"))

(defun config-lang-client-cquery ()
  (when (eq (lang-server-client) 'eglot)
    (with-eval-after-load "eglot"
      (setq eglot-auto-display-help-buffer t)
      (add-to-list 'eglot-server-programs
		   '((c++-mode c-mode) . (eglot-cquery "cquery"))))))

(defun config-cquery ()
  (if (conflict-features current-scope 'ccls)
      (progn
	(message "C/C++ language server `ccls' has already been enabled in scope %s `cquery' language server will be disabled"
		 current-scope)
	nil)
    (progn
      (config-lang-client-cquery)
      t)))

(defun cquery/lsp-on ()
  (progn
    (if (feature-enabled 'yasnippet)
	(setq lsp-enable-snippet t)
      (setq lsp-enable-snippet nil))
    (let ((feature-actived-scope (feature-enabled 'lsp-ui)))
      (when (member current-scope feature-actived-scope)
	(add-hook 'lsp-mode-hook 'lsp-ui-mode)))
    (enable-lsp)))

(defun cquery/eglot-on ()
  (require 'eglot)
  (enable-eglot))

(defun cquery-on ()
  (require 'cquery)
  (cond
   ((eq (lang-server-client) 'eglot) (cquery/eglot-on))
   (t (cquery/lsp-on)))
  t)

(defun cquery-pkgs ()
  (let ((pkgs (cond
	       ((eq (lang-server-client) 'eglot) '(eglot emacs-cquery))
	       (t '(lsp-module emacs-cquery)))))
    pkgs))

(feature! cquery
	  "Use cquery as c/c++ language server"
	  cquery-pkgs
	  config-cquery
	  cquery-on
	  nil)
