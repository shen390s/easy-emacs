;;; core-package.el --- description -*- lexical-binding: t; -*-

(defvar bootstrap-version)

(defun bootstrap-straight ()
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)
    (straight-use-package 'use-package)))

(defun bootstrap-package (pkgmgr)
  (cond
   ((string= pkgmgr "straight") (bootstrap-straight))
   (t (bootstrap-straight))))

(defun install-packages(pkginfo)
  t)

(provide 'core-package)
;;; core-package.el ends here
