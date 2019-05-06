;;; init.el --- description -*- lexical-binding: t; -*-
;;

(require 'core (concat user-emacs-directory "core/core"))

(bootstrap-package "straight")
(load-modules easy-emacs-modules-dir)
;; Enable features
(enable! emacs-server
	 which-key
	 ivy
	 undo-tree
	 evil
	 yasnippet
	 magit
	 (c-c++ smartparens cquery rainbow-delimiters
		rainbow-identifiers
		linum)
	 (elisp smartparens rainbow-delimiters
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
