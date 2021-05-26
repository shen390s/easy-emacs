(package! ergoemacs
	     "ErgoEmacs"
             (ergoemacs :type git
			:host github
			:repo "ergoemacs/ergoemacs-mode"))

(defun config-ergoemacs (scope &optional phase options)
  (setq ergoemacs-theme nil)
  (setq ergoemacs-keyboard-layout "us"))

(defun activate-ergoemacs (scope &optional phase options)
  (require 'ergoemacs-mode)
  (pcase scope
    ('app (let ((status (plist-get options :status)))
	    (if (and status
		     (>= status 0))
		(progn
		  (ergoemacs-mode 1))
	      (progn
		(ergomacs-mode -1)))))
    (_ t)))

(feature! ergoemacs
	  "ErgoEmacs"
          (ergoemacs)
          config-ergoemacs
	  nil
          activate-ergoemacs)
