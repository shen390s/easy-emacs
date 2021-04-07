;; -*- lexical-binding: t -*-

(require 'core-lib)
(require 'core-log)
(require 'core-modules)
(require 'core-features)
(require 'core-scope)

(defun easy-emacs-configure (args)
  ;; put list of features to scope
  (let ((config (collect-keyword-values args)))
    (DEBUG! "config = %s" config)
    (let ((scope-keywords (filter-out-non-keywords config)))
      (DEBUG! "scope keywords = %s" scope-keywords)
      (cl-loop for key in scope-keywords
	       do (with-scope! (intern (format "%s" (keyword-name key)))
			       scope
			       (Scope/parse-configs scope (plist-get config key))))
      ;; configure scope
      (cl-loop for key in scope-keywords
	       do (with-scope! (intern (format "%s" (keyword-name key)))
			       scope
			       (DEBUG! "Configure scope %s" scope)))
      ;; deactivate disabled
      (cl-loop for key in scope-keywords
	       do (with-scope! (intern (format "%s" (keyword-name key)))
			       scope
			       (DEBUG! "disable scope %s features"
				       scope)))
      ;; activate scope features
      (cl-loop for key in scope-keywords
	       do (with-scope! (intern (format "%s" (keyword-name key)))
			       scope
			       (DEBUG! "enable scope %s features"
				       scope)))
      t)))

;; (easy! :modes (c c++ markdown)
;;        :ui (my-theme)
;;        :config ((a. 10) (b . 20))
;;        :completion (ivy))

(defmacro easy! (&rest args)
  `(easy-emacs-configure ',args))

(provide 'core-config)
