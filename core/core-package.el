;; -*- lexical-binding: t -*-
;; enable lexical scope
;;; core-package.el 

(require 'cl-lib)
(require 'core-lib)
(require 'core-log)

(defvar bootstrap-version)

(defun bootstrap-straight ()
  (let ((bootstrap-file
         (expand-file-name
	  "straight/repos/straight.el/bootstrap.el"
	  user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (let ((local-bootstrap-file (concat easy-emacs-dir "/bootstrap/install.el")))
        (message "local bootstrap file %s" local-bootstrap-file)
	(let ((buffer (get-buffer-create "*straight bootstrap*")))
	  (with-current-buffer buffer
            (insert-file-contents local-bootstrap-file)
            (goto-char (point-max))
            (eval-print-last-sexp)))))
    (progn 
        (load bootstrap-file nil 'nomessage))))

(defun install-pkg (pkg)
  (when pkg
    (progn
      (when (fboundp 'straight-use-package)
	(if (listp pkg)
	    (straight-use-package pkg)
	  (straight-use-package pkg))))))

(defun package-patches (pkg)
  (let ((pkg-patch-dir (format "%s/patches/%s"
			       easy-emacs-dir
			       pkg))
	(patch-file-rexp ".*\\.diff"))
    (when (file-accessible-directory-p pkg-patch-dir)
      (let ((files (directory-files pkg-patch-dir
				    nil
				    patch-file-rexp)))
	(cl-loop for file in (sort files 'string<)
		 collect (format "%s/%s"
				 pkg-patch-dir
				 file))))))

;; apply patch
(defvar patch-command nil
  "where to find the executable of patch command")

(defun apply-patch (patch-command dir patch)
  (let ((patch-status-file (format "%s/.%s.patched" dir
				   (file-name-nondirectory patch))))
    (if (file-exists-p patch-status-file)
	0
      (if (shell-command (format "cd %s &&  %s < %s && touch .%s.patched"
				 dir patch-command patch
				 (file-name-nondirectory patch))
			 "PATCH output"
			 "PATCH errors")
	  1
	-1))))

(defun apply-patches (dir patches)
  (DEBUG! "apply-patches dir %s patches %s"
	  dir patches)
  (unless patch-command
    (setq patch-command
	  (executable-find "patch"))) 

  (when patch-command
    (cl-loop for patch in patches
	     do (apply-patch patch-command
			     dir
			     patch))))

(defun apply-package-patches (pkg patches)
  (DEBUG! "apply-package-patches pkg %s patches %s"
	  pkg patches)
  (let ((dir (expand-file-name (format "straight/repos/%s" pkg)
			       user-emacs-directory)))
    (apply-patches dir patches)))

(defun install-core-packages (pkgs)
  (progn
    (cl-loop for pkg in pkgs
	     do (install-pkg pkg))))

(defun bootstrap-package (pkgmgr)
  (cond
   ((string= pkgmgr "straight") (bootstrap-straight))
   (t (bootstrap-straight))))

(provide 'core-package)
;;; core-package.el ends here
