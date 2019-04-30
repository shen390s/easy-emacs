;;; core.el --- description -*- lexical-binding: t; -*-
(defvar easy-emacs-dir (file-truename user-emacs-directory)
  "Directory for easy-emacs")

(defvar easy-emacs-core-dir (concat easy-emacs-dir "core")
  "Directory of easy-emacs core components")

(defvar easy-emacs-modules-dir (concat easy-emacs-dir "modules")
  "Directory of easy-emacs modules")

(require 'core-package (concat easy-emacs-core-dir "/core-package"))
(require 'core-modules (concat easy-emacs-core-dir "/core-modules"))
(require 'core-features (concat easy-emacs-core-dir "/core-feartures"))

(provide 'core)
;;; core.el ends here
