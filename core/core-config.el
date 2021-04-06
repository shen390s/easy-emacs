;; -*- lexical-binding: t -*-

(require 'core-lib)
(require 'core-log)
(require 'core-modules)
(require 'core-features)
(require 'core-scope)

(defun call-config (key val)
  (let ((fn (get-config-fn key)))
    (if (fboundp fn)
	(funcall fn val)
      (WARN! "No keyword defined for configuration %s value %s"
	     key val))))

(defun use-feature-in-scope (scope feature)
  t)

(defun easy-emacs-configure (config)
  ;; put list of features to scope
  (cl-loop for key in scope-keywords
	   do (with-scope! (format ("%s-scope" (keyword-name key)))
			   scope
			   (cl-loop for feature in (plist-get config
							      key)
				    (use-feature-in-scope scope feature))))
  ;; configure scope
  ;; deactivate disabled
  ;; activate scope features
  t)

;; (easy! :modes (c c++ markdown)
;;        :ui (my-theme)
;;        :config ((a. 10) (b . 20))
;;        :completion (ivy))

(defmacro easy! (&rest args)
  `(easy-emacs-configure ',args))

(provide 'core-config)
