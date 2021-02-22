;; -*- lexical-binding: t -*-
;; enable lexical scope
;;; core-package.el 

(defvar bootstrap-version)

(defvar all-packages-ready-hook nil
  "Hook function will run after all packages have been installed")

(defun run-after-all-package-install (func)
  (add-hook 'all-packages-ready-hook func))

(defun do-patch (dir patch)
  (let ((msg (shell-command-to-string 
                (format "cd %s && patch <%s"
                        dir patch))))
      (message "%s\n" msg)))

(defun patch-straight ()
  (let ((patch-dir (concat easy-emacs-dir "/patches"))
        (straight-dir (concat user-emacs-directory "/straight/repos/straight.el")))
       (progn 
            (do-patch straight-dir (concat patch-dir "/01-use-cnpmjs-mirror.diff")) 
            t)))

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
    (progn 
        (patch-straight)
        (load bootstrap-file nil 'nomessage))))

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

(provide 'core-package)
;;; core-package.el ends here
