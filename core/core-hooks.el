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

;; Deferred run after easy-emacs boot
(defvar run-after-boot-list nil
  "List of features to be activated after easy emacs boot")

(defun run-after-boot ()
  (cl-loop for feature in run-after-boot-list
	   do (let ((handler (car feature))
		    (feature-name (car (cdr feature)))
		    (options (cdr (cdr feature))))
		(funcall handler feature-name options))))

(defun after-boot-run (handler args1 args2)
  (add-to-list 'run-after-boot-list
	       `(,handler . (,args1 . ,args2))))


(after-boot! run-after-boot)

(provide 'core-hooks)
;;; core-hooks.el ends here

