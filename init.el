;; -*- lexical-binding: t; -*-
;; Configuration file
;; for easy-emacs

(defvar easy-emacs-dir (file-truename user-emacs-directory)
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

(byte-recompile-directory easy-emacs-core-dir 0)

(require 'core)

(let ((easy-emacs-config-file (concat easy-emacs-config-dir "/" easy-emacs-config)))
  (progn
    (unless (file-exists-p easy-emacs-config-file)
      (copy-file (concat easy-emacs-dir "/" easy-emacs-config)
                 easy-emacs-config-file))
    (easy-emacs-bootstrap-core)
    (easy-emacs-bootstrap easy-emacs-modules-dir
                          easy-emacs-config-file)))

;; Enter global scope
(enter-global)

;;; init.el ends here
