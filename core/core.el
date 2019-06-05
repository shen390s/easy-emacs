;; -*- lexical-binding: t; -*-
;; core.el

(defvar easy-emacs-dir (file-truename user-emacs-directory)
  "Directory for easy-emacs")

(defvar easy-emacs-core-dir (concat easy-emacs-dir "core")
  "Directory of easy-emacs core components")

(defvar easy-emacs-modules-dir (concat easy-emacs-dir "modules")
  "Directory of easy-emacs modules")

(defvar easy-emacs-etc-dir (concat easy-emacs-dir ".local/etc/")
  "Directory of customized easy-emacs settings")

(unless (file-exists-p easy-emacs-etc-dir)
  (make-directory easy-emacs-etc-dir t))

(defvar easy-emacs-file-name-handler-alist file-name-handler-alist)

(defvar easy-emacs-boot-done-hook nil
  "Hooks which will be called when easy-emacs boot done")

(defvar easy-emacs-boot-ok nil)

(defun easy-emacs-boot ()
  (setq gc-cons-threshold 402653184
	gc-cons-percentage 0.6
	file-name-handler-alist nil))

(add-hook 'easy-emacs-boot-done-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216
		  gc-cons-percentage 0.1
		  file-name-handler-alist easy-emacs-file-name-handler-alist
		  easy-emacs-boot-ok t)))

(defun easy-emacs-boot-done ()
  (unless easy-emacs-boot-ok
    (run-hooks 'easy-emacs-boot-done-hook)))

(easy-emacs-boot)
(require 'core-lib (concat easy-emacs-core-dir "/core-lib"))
(require 'core-keybind (concat easy-emacs-core-dir "/core-keybind"))
(require 'core-package (concat easy-emacs-core-dir "/core-package"))
(require 'core-features (concat easy-emacs-core-dir "/core-features"))
(require 'core-modules (concat easy-emacs-core-dir "/core-modules"))

(setq custom-file (concat easy-emacs-etc-dir "custom.el"))

(provide 'core)
;;; core.el ends here
