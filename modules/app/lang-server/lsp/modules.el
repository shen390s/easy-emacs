(package! lsp-module
	  "Emacs lisp client to language server"
	  lsp-mode)

(package! lsp-ui
	  "This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses."
	  (lsp-ui :type git
		  :host github
		  :repo "emacs-lsp/lsp-ui"))

(defvar lsp-remap-xref-keybindings t
  "When non-nil, xref keybindings remapped to lsp-ui-peek-find-*")

(defun lsp-setup-keybind ()
  (when (feature-enabled 'evil)
    (require 'evil)
    (when lsp-remap-xref-keybindings
      (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
      (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
    
    (define-key-in-major-map
      "gd" #'lsp-find-definition
      "gr" #'lsp-find-references
      "gi" #'lsp-find-implementation
      "gt" #'lsp-find-type-definition)
    
    (easy-emacs/define-key lsp-ui-peek-mode-map
			   "h" #'lsp-ui-peek--select-prev-file
			   "j" #'lsp-ui-peek--select-next
			   "k" #'lsp-ui-peek--select-prev
			   "l" #'lsp-ui-peek--select-next-file)))

(defun enable-lsp ()
  (progn
    (lsp)
    (lsp-setup-keybind)))

(feature! lsp
	  "Enable lsp mode"
	  (lsp-module)
	  nil
	  nil 
	  nil)

(feature! lsp-ui
	  "This contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses."
	  (lsp-ui)
	  nil
	  nil
	  nil)
