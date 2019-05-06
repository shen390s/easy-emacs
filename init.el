;;; init.el --- description -*- lexical-binding: t; -*-
;;

(require 'core (concat user-emacs-directory "core/core"))

(bootstrap-package "straight")
(load-modules easy-emacs-modules-dir)
;; Enable features
;;(enable! )
(load custom-file t t)

;; Enable features
(enable! emacs-server
	 which-key
	 ivy
	 (c-c++ cquery rainbow-delimiters
		rainbow-identifiers)
	 (elisp smartparens rainbow-delimiters
		rainbow-identifiers))

;; Install actived packages

(install-packages
 (pkglist-info
  (actived-packages
   (actived-features))))

;; build scope hooks
(build-hooks)

;; Enter global scope
(enter-scope 'global)

;;; init.el ends here
