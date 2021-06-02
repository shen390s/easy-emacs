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

(defun apply-patch (dir patch)
  (let ((patch-status-file (format "%s/.%s.patched" dir
				   (file-name-nondirectory patch))))
    (if (file-exists-p patch-status-file)
	0
      (if (shell-command (format "cd %s &&  %s < %s && touch %s"
				 dir patch-command patch
				 patch-status-file)
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
	     do (apply-patch dir
			     patch))))

(defun pkg-dir (pkg)
  (let ((pkginfo (cdr (with-slots (pkg-info) (get-package pkg)
			pkg-info))))
    (if (and (listp pkginfo)
	     pkginfo)
	(file-name-nondirectory (plist-get pkginfo :repo))
      pkg)))

(defun apply-package-patches (pkg patches)
  (DEBUG! "apply-package-patches pkg %s patches %s"
	  pkg patches)
  (let ((dir (expand-file-name (format "straight/repos/%s"
				       (pkg-dir pkg))
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

(defclass Package ()
  ((name :initarg :name
	 :initform "Anonymous")
   (docstring :initarg :docstring
	      :initform "")
   (pkg-info :initarg :pkg-info
	     :initform nil)
   (patches :initarg :patches
	    :initform nil)
   (mutex :initarg :mutex
	  :initform nil)
   (installed :initarg :installed
	      :initform nil))
  "Class to describe the package of Emacs")

(cl-defmethod Package/install ((pkg Package))
  (with-slots (name installed pkg-info mutex) pkg
    (unless installed
      (when (fboundp 'install-pkg)
	(with-mutex mutex
	  (install-pkg pkg-info)
	  (setf installed t))))))

(cl-defmethod Package/apply_patches ((pkg Package))
  (with-slots (name patches) pkg
    (DEBUG! "Package/apply_patches %s"
	    pkg)
    (unless patches
      (setf patches (package-patches name)))
    (when patches
      (apply-package-patches name patches))))

(cl-defmethod Object/to-string ((obj Package))
  (pp-to-string obj))

(defvar all-packages (make-hash-table)
  "All defined packages")

(defvar all-features (make-hash-table)
  "All defined features")

(defmacro package! (name docstring pkginfo)
  (declare (doc-string 2))
  `(let ((patches (package-patches ',name)))
     (let ((package (make-instance 'Package
				   :name ',name
				   :docstring ',docstring
				   :pkg-info ',pkginfo
				   :patches patches
				   :mutex (make-mutex (format "%s/:mutex" ',name))
				   :installed nil)))
       (progn
	 (puthash ',name package all-packages)))))

(defun do-package-install (pkg)
  (DEBUG! "do-package-install %s..." pkg)
  (let ((package (gethash pkg all-packages)))
    (when package
      (Package/install package))))

(defvar deferred-package-list nil
  "list of packages to be installed later ")

(defvar deferred-package-list-mutex (make-mutex "deferred-package-list")
  "mutex to protect deferred-package-list")

(defvar deferred-package-list-waiter (make-condition-variable deferred-package-list-mutex)
  "condition variable to be wait when background installer thread is idle")

(defun install-package (pkg &optional deferred)
  (if deferred
      (progn
	(with-mutex deferred-package-list-mutex
	  (setf deferred-package-list
		(delete-dups (append deferred-package-list
				     (list pkg))))
	  (condition-notify deferred-package-list-waiter)))
    (do-package-install pkg)))

(defun install-packages (pkgs &optional deferred)
  (DEBUG! "install-packages %s deferred %s"
	  pkgs deferred)
  (cl-loop for pkg in pkgs
	   do (install-package pkg deferred)))

(defun get-package (pkg)
  (let ((pkg (if (symbolp pkg)
		 pkg
	       (intern (format "%s" pkg)))))
    (let ((package (gethash pkg all-packages)))
      (unless package
	(setq package (Package :name pkg
			       :pkg-info pkg
			       :installed nil))
	(puthash pkg package all-packages))
      (DEBUG2! "get-package %s"
	       (Object/to-string package))
      package)))

;; enable package patches when enable/install
;; packages
(add-hook 'straight-use-package-prepare-functions
	  #'(lambda (pkg-name)
	      (DEBUG! "prepare pkg %s" pkg-name)
	      (let ((pkg (get-package pkg-name)))
		(when pkg
		  (Package/apply_patches pkg)))))

(defvar background-package-installer-should-go nil
  "set to t when background-package-installer should go")

(add-hook 'after-init-hook
	  #'(lambda ()
	      (start-background-package-installer)))

(defun start-background-package-installer ()
  (let ((installer-thread (make-thread (lambda ()
					 (DEBUG! "eay emacs background package installer is running")
					 (while (not background-package-installer-should-go)
					   (when (emacs-idle-p)
					     (let ((pkg (with-mutex deferred-package-list-mutex
							  (if (null deferred-package-list)
							      (condition-wait deferred-package-list-waiter)
							    (let ((pkg1 (car deferred-package-list)))
							      (setf deferred-package-list
								    (cdr deferred-package-list))
							      pkg1)))))
					       (when pkg
						 (DEBUG! "I will install package %s"
							 pkg)
						 (do-package-install pkg))))
					   (thread-yield))))))
    (add-hook 'kill-emacs-hook
	      #'(lambda ()
		  (setf background-package-installer-should-go t)
		  (with-mutex deferred-package-list-mutex
		    (condition-notify deferred-package-list-waiter))
		  (thread-join installer-thread)))))

(provide 'core-package)
;;; core-package.el ends here
