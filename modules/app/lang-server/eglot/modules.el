(package-ex! eglot
	     "A client for Language Server Protocol servers "
	     (eglot :type git
		    :host github
		    :repo "joaotavora/eglot"))

(defun activate-eglot (scope &optional phase options)
  (require 'eglot)

  (let ((status (plist-get options :status)))
    (if (and status
	     (>= status 0))
	(eglot-ensure)
      t)))

(feature-ex! eglot
	     "A client for Language Server Protocol server"
	     (eglot)
	     nil
	     nil ;; eglot will be enabled indirectly by cquery/ccls
	     activate-eglot)
