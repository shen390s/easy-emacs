;;; init.el --- description -*- lexical-binding: t; -*-
;;

(require 'core (concat user-emacs-directory "core/core"))

(bootstrap-package "straight")
(load-modules easy-emacs-modules-dir)
;; Enable features
;;(enable! )
(load custom-file t t)

(enable! emacs-server)

;; Enter global scope
(enter-scope 'global)

;;; init.el ends here
