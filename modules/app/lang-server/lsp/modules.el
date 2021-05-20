;; -*- lexical-binding: t -*-

(package-ex! lsp-module
	     "Emacs lisp client to language server"
	     lsp-mode)

(package-ex! lsp-ui
	     "This contains all the higher level UI modules of
	     lsp-mode,like flycheck support and code lenses."
	     (lsp-ui :type git
		     :host github
		     :repo "emacs-lsp/lsp-ui"))

(defun config-lsp (scope &optional phase options)
  (pcase phase
    (:check
     (after-activate! evil
		      (progn
			(DEBUG! "config-lsp scope %s phase %s options %s"
				scope phase options)
			(evil-leader/set-key-for-mode (get-mode-from-options options)
			  (kbd "mld") #'lsp-find-definition
			  (kbd "mlr") #'lsp-find-references
			  (kbd "mli") #'lsp-find-implemention
			  (kbd "mlt") #'lsp-find-type-definition))))
    (_ t)))

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
    (_ t)))

(feature-ex! lsp
	     "Enable lsp mode"
	     (lsp-module lsp-ui)
	     config-lsp
	     prepare-lsp 
	     activate-lsp)

