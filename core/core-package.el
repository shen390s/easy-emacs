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
    (load bootstrap-file nil 'nomessage)))

(defun install-pkg (pkg)
  (when pkg
    (progn
      (when (fboundp 'straight-use-package)
	(if (listp pkg)
	    (straight-use-package pkg)
	  (straight-use-package pkg))))))

(defun install-core-packages (pkgs)
  (progn
    (cl-loop for pkg in pkgs
	     do (install-pkg pkg))))

(defun bootstrap-package (pkgmgr)
  (cond
   ((string= pkgmgr "straight") (bootstrap-straight))
   (t (bootstrap-straight))))

(defun install-packages(pkginfo)
  (DEBUG! "Install packages: %s" pkginfo)
  (progn
    (cl-loop for pkg in pkginfo
	     do (install-pkg pkg))
    (run-hooks 'all-packages-ready-hook)))

;; autoload-r! will enabled a function in a package
;; which has not already been installed to be
;; autoloaded
(defmacro autoload-r! (fn pkgs filename &optional interactive)
  `(defun ,fn (&rest args)
     ,(when interactive
	`(interactive))
     ,@(cl-loop for pkg in pkgs
		collect `(install-pkg ',pkg))
     (let ((my-self (symbol-function ',fn)))
       (fmakunbound ',fn)
       (if (load-library ,filename)
	   (apply ',fn args)
	 (fset ',fn my-self)))))

(provide 'core-package)
;;; core-package.el ends here
