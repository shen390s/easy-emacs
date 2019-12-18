;; -*- lexical-binding: t; -*-
;; Configuration file
;; for easy-emacs

(defvar easy-emacs-dir (file-truename user-emacs-directory)
  "Directory for easy-emacs")

(defvar easy-emacs-core-dir (concat easy-emacs-dir "/core")
  "Directory of easy-emacs core components")

(defvar easy-emacs-modules-dir (concat easy-emacs-dir "/modules")
  "Directory of easy-emacs modules")

(defvar easy-emacs-etc-dir (concat easy-emacs-dir "/.local/etc/")
  "Directory of customized easy-emacs settings")

(push easy-emacs-core-dir load-path)

(byte-recompile-directory easy-emacs-core-dir 0)

(require 'core)

(easy-emacs-bootstrap-core)

(easy-emacs-bootstrap easy-emacs-modules-dir
		      (concat easy-emacs-config-dir
			      "/easy-emacs-config.el"))
;; Enter global scope
(enter-global)

;;; init.el ends here
