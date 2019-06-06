;; -*- lexical-binding: t; -*-
;; Configuration file
;; for easy-emacs

(require 'core (concat user-emacs-directory "core/core"))

(easy-emacs-bootstrap-core)

(easy-emacs-bootstrap easy-emacs-modules-dir
		      (concat user-emacs-directory
			      "easy-emacs-config.el"))
;; Enter global scope
(enter-scope 'global)

;; load customization
(load custom-file t t)

;;; init.el ends here
