(package! smex
	     "Smex is a M-x enhancement for Emacs"
	     (smex :type git
		   :host github
		   :repo "nonsequitur/smex"))

(defun smex-bind-keys ()
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(defun activate-smex (scope &optional phase options)
  (DEBUG! "activate-smex scope %s phase %s options %s"
	  scope phase options)
  
  (require 'smex)
  (let ((status (plist-get options :status)))
    (if (>= status 0)
	(progn
	  (smex-initialize)
	  (smex-bind-keys))
      t)))

(feature! smex
	     "Smex is a M-x enhancement for Emacs"
	     (smex)
	     nil
	     nil
	     activate-smex)
