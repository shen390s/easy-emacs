(package!  powerline
	      "Emacs version of the VIM powerline"
	      (powerline :type git
			 :host github
			 :repo "milkypostman/powerline"))

(defun activate-powerline (scope &optional phase options)
  (require 'powerline)
  (let ((status (plist-get options :status)))
    (when (and status
	       (>= status 0))
      (powerline-default-theme))))

(feature! powerline
	     "Emacs version of the VIM powerline"
	     (powerline)
	     nil
	     nil
	     activate-powerline)
	     
