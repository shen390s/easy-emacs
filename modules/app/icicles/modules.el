(package! icicles
	     "Minibuffer input completion and cycling"
	     (icicles :type git
		      :host github
		      :repo "emacsmirror/icicles"))


(defun activate-icicles (scope &optional phase options)
  (require 'icicles)

  (condition-case err
      (let ((status (plist-get options :status)))
	(when status
	  (if (>= status 0)
	      (icy-mode 1)
	    (icy-mode -1))))
    (error (INFO! "active icciles failed: %s"
                  (error-message-string err))
           nil)))

(feature! icicles
	     "minibuffer input completion and cycling"
	     (icicles)
	     nil
	     nil
	     activate-icicles)
