;; -*- lexical-binding: t; -*-
;; core.el

(require 'core-lib)
(require 'core-log)
(require 'core-hooks)
(require 'core-keybind)
(require 'core-package)
(require 'core-features)
(require 'core-modules)
(require 'core-config)

(defconst easy-emacs-version
  "0.2"
  "The version of EasyEmacs")

(unless (file-exists-p easy-emacs-config-dir)
  (make-directory easy-emacs-config-dir t))

(defvar easy-emacs-file-name-handler-alist
  file-name-handler-alist)

(defvar easy-emacs-boot-ok nil)

(defvar easy-emacs-core-packages
  (list 'use-package
	'(bind-map :type git
		   :host github
		   :repo "justbur/emacs-bind-map")
	'el-patch)
  "list of packages which will be used by easy-emacsi core")

(defvar easy-emacs-deferred-packages nil
  "List of deferred installed packages")

(defvar easy-emacs-idle-package-installation-timer nil
  "Timer to trigger package to be installed")

(defvar easy-emacs-idle-time (* 60 15)
  "Idle time(seconds) to trigger deferred package installation")

(defun my-branch ()
  (let ((dir (file-name-directory load-file-name)))
    (git-branch dir)))

(defun my-version ()
  (format "%s-%s"
	  easy-emacs-version
	  (my-branch)))

(defun easy-emacs-boot-start ()
  (setq gc-cons-threshold 402653184
	gc-cons-percentage 0.6
	file-name-handler-alist nil)
  ;; Initialize log system
  ;;
  (log-init! DEBUG)
  (INFO! "Booting EasyEMACS[%s]..."
	 (my-version)))

(add-hook 'easy-emacs-boot-done-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216
		  gc-cons-percentage 0.1
		  file-name-handler-alist easy-emacs-file-name-handler-alist
		  easy-emacs-boot-ok t)))

(defun easy-emacs-boot-done ()
  (unless easy-emacs-boot-ok
    (run-hooks 'easy-emacs-boot-done-hook))
  (INFO! "EasyEMACS[%s] boot OK"
	 (my-version)))


(defun easy-emacs-bootstrap-core ()
  (bootstrap-package "straight")
  (install-core-packages easy-emacs-core-packages))

(defun schedule-package-defer-installation ()
  (setf easy-emacs-idle-package-installation-timer
	(run-with-idle-timer easy-emacs-idle-time (1+ (length easy-emacs-deferred-packages))
			     (lambda ()
			       (if easy-emacs-deferred-packages
				   (let ((pkg (pop easy-emacs-deferred-packages)))
				     (progn
				       (install-package-by-name pkg)))
				 (progn
				   (when easy-emacs-idle-package-installation-timer
				     (cancel-timer easy-emacs-idle-package-installation-timer)
				     (setf easy-emacs-idle-package-installation-timer nil))))))))


(defun easy-emacs-bootstrap (module-dir config)
  (load-modules module-dir)

  ;; load configuration of
  ;; easy-emacs
  (load-file config)
  (add-hook 'easy-emacs-boot-done-hook
	    ;; works to do after bootstrap
	    (lambda ()
	      t)))

(defun defer-package-install (pkgs)
  (setf easy-emacs-deferred-packages
	(delete-dups (append pkgs
			     remote-autoload-pkgs)))
  (DEBUG! "Defer to install packages: %s"
	  easy-emacs-deferred-packages)
  (unless easy-emacs-idle-package-installation-timer
    (schedule-package-defer-installation)))

(easy-emacs-boot-start)

(provide 'core)
;;; core.el ends here
