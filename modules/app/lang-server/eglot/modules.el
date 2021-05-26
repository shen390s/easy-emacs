(package! eglot
	     "A client for Language Server Protocol servers "
	     (eglot :type git
		    :host github
		    :repo "joaotavora/eglot"))

(defun activate-eglot (scope &optional phase options)
  (require 'project)
  (require 'eglot)

  (unless (fboundp 'project-root)
    (defun project-root (project)
      (car (project-roots project))))
  
  (let ((status (plist-get options :status)))
    (if (and status
	     (>= status 0))
	(pcase scope
	  ('modes (progn
		    (when (feature-off :flymake options)
		      (setq eglot-disable-flymake t))
		    (when (feature-off :eldoc options)
		      (setq eglot-disable-eldoc t))
		    (eglot-ensure)))
	  (_ t))
      t)))

(feature! eglot
	     "A client for Language Server Protocol server"
	     (project eglot)
	     nil
	     nil ;; eglot will be enabled indirectly by cquery/ccls
	     activate-eglot)
