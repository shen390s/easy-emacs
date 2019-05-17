(package! eglot
	  "A client for Language Server Protocol servers "
	  (eglot :type git
		 :host github
		 :repo "joaotavora/eglot"))

(defun config-eglot ()
  t)

(defun enable-eglot ()
  (eglot-ensure))

(feature! eglot
	  "A client for Language Server Protocol server"
	  (eglot)
	  config-eglot
	  nil ;; eglot will be enabled indirectly by cquery/ccls
	  nil)
