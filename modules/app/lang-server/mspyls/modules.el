(package-ex! lsp-python-ms
	     "emacs lsp-mode client for Microsoft's python language server "
	     (lsp-python-ms :type git
			    :host github
			    :repo "emacs-lsp/lsp-python-ms"))

(defun activate-lsp-python-ms (scope &optional phase options)
  (require 'lsp)
  (require 'lsp-ui)
  (require 'lsp-diagnostics)
  (require 'lsp-python-ms)
  (let ((status (plist-get options :status)))
    (if (and status
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

(defun deactivate-lsp-python-ms ()
  (lsp-mode -1))

(feature-ex! lsp-python-ms
	     "Emacs lsp-mode client for Microsoft python language server"
	     (lsp-module lsp-python-ms)
	     nil
	     nil
	     activate-lsp-python-ms)
