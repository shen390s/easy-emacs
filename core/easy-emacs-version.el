;; -*- lexical-binding: t; -*-
;; easy-emacs-version.el

(defvar easy-emacs-version "0.1"
  "The version of EasyEmacs")

(defun my-branch ()
  (let ((easy-emacs-dir
	 (format "%s/.."
		 (file-name-directory
		  load-file-name))))
    (let ((cmd (format "GIT_DIR=%s/.git git branch --show-current"
		       easy-emacs-dir)))
      (let ((br (shell-command-to-string cmd)))
	(if (> (length br) 0)
	    br
	  "unknown")))))

