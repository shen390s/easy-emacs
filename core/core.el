;;; core.el --- description -*- lexical-binding: t; -*-
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

(require 'core-package (concat easy-emacs-core-dir "/core-package"))
(require 'core-features (concat easy-emacs-core-dir "/core-features"))
(require 'core-modules (concat easy-emacs-core-dir "/core-modules"))

(setq custom-file (concat easy-emacs-etc-dir "custom.el"))

(provide 'core)
;;; core.el ends here
