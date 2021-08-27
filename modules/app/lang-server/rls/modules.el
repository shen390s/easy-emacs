;; -*- lexical-binding: t -*-
(defun activate-rls (scope &optional phase options)
  (require 'lsp)
  (require 'lsp-diagnostics)

  (let ((status (plist-get options :status)))
    (if (and  status
	      (>= status 0))
	(pcase scope
	  ('modes (progn
		    (cond
		     ((and (feature-on :flymake options)
			   (feature-on :flycheck options))
		      (setq lsp-diagnostics-provider :auto))
		     ((and (feature-off :flymake options)
			   (feature-off :flycheck options))
		      (setq lsp-diagnostics-provider :none))
		     ((feature-on :flymake options)
		      (setq lsp-diagnostics-provider :flymake))
		     (t
		      (setq lsp-diagnostics-provider :flycheck)))
		    (DEBUG! "activate-lsp lsp-diagnostics-provider %s"
			    lsp-diagnostics-provider)
		    (lsp)))
	  (_ t))
      (lsp-mode -1))))

(feature! rls
	     "Rust language server"
	     (lsp-module)
	     nil
	     nil
	     activate-rls)
