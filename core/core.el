;; -*- lexical-binding: t; -*-
;; core.el

(unless (file-exists-p easy-emacs-etc-dir)
  (make-directory easy-emacs-etc-dir t))

(defvar easy-emacs-file-name-handler-alist file-name-handler-alist)

(defvar easy-emacs-boot-done-hook nil
  "Hooks which will be called when easy-emacs boot done")

(defvar easy-emacs-boot-ok nil)

(defvar easy-emacs-core-packages
  (list 'use-package
	'(bind-map :type git
		   :host github
		   :repo "justbur/emacs-bind-map"))
  "list of packages which will be used by easy-emacs")

(defun easy-emacs-boot-start ()
  (setq gc-cons-threshold 402653184
	gc-cons-percentage 0.6
	file-name-handler-alist nil))

(add-hook 'easy-emacs-boot-done-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216
		  gc-cons-percentage 0.1
		  file-name-handler-alist easy-emacs-file-name-handler-alist
		  easy-emacs-boot-ok t)))

(defun easy-emacs-boot-done ()
  (unless easy-emacs-boot-ok
    (run-hooks 'easy-emacs-boot-done-hook)))

(require 'core-lib (concat easy-emacs-core-dir "/core-lib"))
(require 'core-keybind (concat easy-emacs-core-dir "/core-keybind"))
(require 'core-package (concat easy-emacs-core-dir "/core-package"))
(require 'core-features (concat easy-emacs-core-dir "/core-features"))
(require 'core-modules (concat easy-emacs-core-dir "/core-modules"))

(setq custom-file (concat easy-emacs-etc-dir "custom.el"))

(defun easy-emacs-bootstrap-core ()
  (bootstrap-package "straight")
  (install-core-packages easy-emacs-core-packages))

(defun easy-emacs-bootstrap (module-dir config)
  (load-modules module-dir)

  ;; load configuration of
  ;; easy-emacs
  (load-file config)

  ;; Install actived packages
  (install-packages
   (pkglist-info
    (actived-packages
     (actived-features)))))

(easy-emacs-boot-start)

(provide 'core)
;;; core.el ends here
