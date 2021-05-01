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

(byte-recompile-directory easy-emacs-core-dir 0)

(defun prepend-env (s env-var)
  (let ((env-val (getenv env-var)))
    (setenv env-var
	    (if env-val
		(format "%s:%s" s env-val)
	      s))))

;; Enable git_best_mirror_tool to use fast mirrors in china
(let ((git-best-mirror-tool-dir (getenv "GIT_BEST_MIRROR_TOOL_DIR")))
  (when git-best-mirror-tool-dir
    (progn
      (setq exec-path (cons git-best-mirror-tool-dir exec-path))
      (prepend-env git-best-mirror-tool-dir "PATH")
      (prepend-env git-best-mirror-tool-dir "EMACSPATH"))))

(require 'core)

(let ((easy-emacs-config-file (concat easy-emacs-config-dir "/" easy-emacs-config)))
  (progn
    (unless (file-exists-p easy-emacs-config-file)
      (copy-file (concat easy-emacs-dir "/" easy-emacs-config)
                 easy-emacs-config-file))
    (easy-emacs-bootstrap-core)
    (easy-emacs-bootstrap easy-emacs-modules-dir
                          easy-emacs-config-file)))

(easy-emacs-boot-done)

;;; init.el ends here
