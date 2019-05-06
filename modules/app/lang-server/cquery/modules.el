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
    (lsp)))

(feature!
 cquery
 "Use cquery as c/c++ language server"
 (lsp-module emacs-cquery)
 nil
 cquery-on
 nil)
