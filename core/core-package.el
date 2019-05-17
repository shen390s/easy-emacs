;; -*- lexical-binding: t -*-
;; enable lexical scope
;;; core-package.el 

(defvar bootstrap-version)

(defvar all-packages-ready-hook nil
  "Hook function will run after all packages have been installed")

(defun run-after-all-package-install (func)
  (add-hook 'all-packages-ready-hook func))

(defun bootstrap-straight ()
  (let ((bootstrap-file
         (expand-file-name
	  "straight/repos/straight.el/bootstrap.el"
	  user-emacs-directory))
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
  (progn
    (cl-loop for pkg in pkginfo
	     do (when pkg
		  (progn
		    (if (listp pkg)
			(straight-use-package pkg)
		      (straight-use-package pkg)))))
    (run-hooks 'all-packages-ready-hook)))

(provide 'core-package)
;;; core-package.el ends here
