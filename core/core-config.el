;; -*- lexical-binding: t -*-

(require 'core-lib)
(require 'core-log)

(defconst easy-emacs-config-keywords
  '(:config :modes :ui :completion :tools)
  "Configuration keywords for easy-emacs")

(defun get-config-fn (key)
  (intern (format "config/%s" key)))

(defun config/:modes (val)
  (DEBUG! "config/:modes %s" val))

(defun config/:config (val)
  t)

(defun config/:ui (val)
  t)

(defun config/:completion (val)
  t)

(defun config/:tools (val)
  t)

(defun call-config (key val)
  (let ((fn (get-config-fn key)))
    (if (fboundp fn)
	(funcall fn val)
      (WARN! "No keyword defined for configuration %s value %s"
	     key val))))

(defun easy-emacs-configure (config)
  (cl-loop for key in easy-emacs-config-keywords
	   do (cl-loop for val in (plist-get config key)
		       do (call-config key val))))

;; (easy! :modes (c c++ markdown)
;;        :ui (my-theme)
;;        :config ((a. 10) (b . 20))
;;        :completion (ivy))

(defmacro easy! (&rest args)
  `(easy-emacs-configure ',args))

(provide 'core-config)
