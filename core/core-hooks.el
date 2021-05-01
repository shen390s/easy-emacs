;; -*- lexical-binding: t -*-
;; easy-emacs hooks

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'core-lib)
(require 'core-log)

(defvar easy-emacs-boot-done-hook nil
  "Hooks which will be called when easy-emacs boot done")

(defvar all-packages-ready-hook nil
  "Hook function will run after all packages have been installed")

(defun run-after-all-package-install (func)
  (add-hook 'all-packages-ready-hook func))

(defmacro after-boot! (fn &rest args)
  `(add-hook 'easy-emacs-boot-done-hook
	     (lambda ()
	       (,fn ,@args))))

(defmacro mode-hook! (mode fn &rest args)
  `(add-hook ',(intern (format "%s-mode-hook" mode))
	     (lambda ()
	       (,fn ,@args))))

(provide 'core-hooks)
;;; core-hooks.el ends here

