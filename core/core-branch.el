;; -*- lexical-binding: t; -*-
;; easy-emacs-version.el

(defconst easy-emacs-branch
  "master"
  "The branch name")

(defun my-branch ()
  (progn
    (DEBUG! "my base directory is %s"
	    (file-name-directory load-file-name))
    easy-emacs-branch))

(provide 'core-branch)
