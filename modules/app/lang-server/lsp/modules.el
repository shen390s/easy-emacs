;; -*- lexical-binding: t -*-

(package! lsp-module
	     "Emacs lisp client to language server"
	     lsp-mode)

(package! lsp-ui
	     "This contains all the higher level UI modules of
	     lsp-mode,like flycheck support and code lenses."
	     (lsp-ui :type git
		     :host github
		     :repo "emacs-lsp/lsp-ui"))

(defun config-lsp (scope &optional phase options)
  t)

(defun bind-lsp-ui-keys ()
  (easy-emacs/define-key lsp-ui-peek-mode-map
			 "h" #'lsp-ui-peek--select-prev-file
			 "j" #'lsp-ui-peek--select-next
			 "k" #'lsp-ui-peek--select-prev
			 "l" #'lsp-ui-peek--select-next-file))

(defun prepare-lsp (scope &optional phase options)
  t)

(defun activate-lsp (scope &optional phase options)
  (DEBUG! "activate-lsp scope %s phase %s options %s"
	  scope phase options)
  (require 'lsp)
  (require 'lsp-ui)
  (require 'lsp-diagnostics)
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

(feature! lsp
	     "Enable lsp mode"
	     (lsp-module lsp-ui)
	     config-lsp
	     prepare-lsp 
	     activate-lsp)

