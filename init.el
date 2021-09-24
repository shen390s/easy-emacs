;;; init.el --- init file for easy-emacs  -*- lexical-binding: t; -*-
;;  Author: Rongsong  Shen <rshen@shenrs.eu>
;;
;;; Commentary:
;;
;; Configuration file for easy-emacs
;;

(defvar easy-emacs-dir
  (let ((dir (getenv "EASYEMACSDIR")))
    (if dir
	dir
      (file-truename user-emacs-directory)))
  "Directory for easy-emacs")

(defvar easy-emacs-core-dir (concat easy-emacs-dir "/core")
  "Directory of easy-emacs core components")

(defvar easy-emacs-modules-dir (concat easy-emacs-dir "/modules")
  "Directory of easy-emacs modules")

(defvar easy-emacs-config "easy-emacs-config.el"
  "Configuration filename(without directory part) for easy-emacs")

(defvar easy-emacs-config-dir
  (let ((easy-emacs-data (getenv "EASYEMACSDATA")))
    (if easy-emacs-data
        easy-emacs-data
      (concat easy-emacs-dir "/.local")))
  "Directory of customized easy-emacs settings")

(defalias 'dynamic-setting-handle-config-changed-event 'ignore)

(unless custom-file
   (setq custom-file (concat easy-emacs-config-dir "/custom.el")))

(push easy-emacs-core-dir load-path)
(push easy-emacs-config-dir custom-theme-load-path)

;;(byte-recompile-directory easy-emacs-core-dir 0)

(require 'core)

(easy-emacs-boot-start easy-emacs-modules-dir
		       easy-emacs-config)

;;; init.el ends here
