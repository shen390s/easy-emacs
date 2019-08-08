(package! smex
	  "Smex is a M-x enhancement for Emacs"
	  (smex :type git
		:host github
		:repo "nonsequitur/smex"))

(defun smex-bind-keys ()
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(defun activate-smex ()
  (require 'smex)
  (smex-initialize)
  (smex-bind-keys))

(feature! smex
	  "Smex is a M-x enhancement for Emacs"
	  (smex)
	  nil
	  activate-smex
	  nil)
