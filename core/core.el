;;; core.el --- description -*- lexical-binding: t; -*-
(defvar easy-emacs-dir (file-truename user-emacs-directory)
  "Directory for easy-emacs")

(defvar easy-emacs-core-dir (concat easy-emacs-dir "/core")
  "Directory of easy-emacs core components")

(defvar easy-emacs-modules-dir (concat easy-emacs-dir "/modules")
  "Directory of easy-emacs modules")

(require 'core-package easy-emacs-core-dir)

(provide 'core)
;;; core.el ends here
