;; -*- lexical-binding: t -*-

(require 'core-lib)
(require 'core-log)
(require 'core-modules)
(require 'core-features)
(require 'core-scope)

;; (easy! :vars (a . 10) (b . 20)
;;        :modes plantuml mermaid
;;        :ui my-theme
;;        :completion ivy -helm -autoc-complete
;;        :app emacs-server)

(defun make-easy-config (configs)
  (let ((config (collect-keyword-values configs)))
    (DEBUG! "config = %s" config)
    (let ((keys (filter-out-non-keywords config)))
      (DEBUG! "scope keywords = %s" keys)
      (let ((c (cl-loop for key in keys
	       collect `,@(make-scope-by-config key
						(plist-get config
							   key)))))
	(DEBUG! "c = %s" c)
	c))))

(defmacro easy! (&rest args)
  `(progn
     ,@(collect-lists nil (make-easy-config args))))

(provide 'core-config)
