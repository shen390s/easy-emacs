;; -*- lexical-binding: t; -*-
;; Configuration file
;; for easy-emacs

(require 'core (concat user-emacs-directory "core/core"))

(bootstrap-package "straight")
(load-modules easy-emacs-modules-dir)

;; load configuration of
;; easy-emacs
(load-file (concat user-emacs-directory "easy-emacs-config.el"))

;; Install actived packages

(install-packages
 (pkglist-info
  (actived-packages
   (actived-features))))

;; Enter global scope
(enter-scope 'global)

;; load customization
(load custom-file t t)

;;; init.el ends here
