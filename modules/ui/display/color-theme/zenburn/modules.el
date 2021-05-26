(package! zenburn-emacs
	     "The Zenburn colour theme ported to Emacs"
	     (zenburn-emacs :type git
			    :host github
			    :repo "bbatsov/zenburn-emacs"))

(defun activate-zenburn-emacs (scope &optional phase options)
  (pcase scope
    ('ui (let ((status (plist-get options :status)))
	   (when (and status
		      (>= status 0))
	     (load-theme 'zenburn t))))
    (_ t)))

(feature! zenburn-emacs
	     "The Zenburn colour theme ported to Emacs"
	     (zenburn-emacs)
	     nil
	     nil
	     activate-zenburn-emacs)
