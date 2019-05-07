;;; init.el --- description -*- lexical-binding: t; -*-
;;

(require 'core (concat user-emacs-directory "core/core"))

(bootstrap-package "straight")
(load-modules easy-emacs-modules-dir)

;; Enable features
;;  features in global scope
(enable! global
	 (emacs-server
	  which-key
	  ivy
	  undo-tree
	  evil
	  yasnippet
	  magit
	  rust
	  golden-ratio))

;; features in c-c++ scope
(enable!  c-c++
	  (smartparens
	   lsp-ui
	   (cquery ((cquery-executable . "/usr/local/bin/cquery")))
	   rainbow-delimiters
	   rainbow-identifiers
	   linum
	   google-c-style))

;; features in elisp scope
(enable! elisp
	 (smartparens
	  rainbow-delimiters
	  rainbow-identifiers
	  linum))

;; features in python scope
(enable! python
	 (smartparens
	  rainbow-delimiters
	  rainbow-identifiers
	  linum))

;; features in rust scope
(enable! rust
	 (smartparens
	  rainbow-delimiters
	  rainbow-identifiers
	  linum))

;; Install actived packages

(install-packages
 (pkglist-info
  (actived-packages
   (actived-features))))

;; build scope hooks
(build-hooks)

;; Enter global scope
(enter-scope 'global)

;; load customization
(load custom-file t t)

;;; init.el ends here
