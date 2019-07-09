(package! ergoemacs
	  "ErgoEmacs"
          (ergoemacs :type git
		     :host github
                     :repo "ergoemacs/ergoemacs-mode"))

(defun config-ergoemacs ()
  t)

(defun enable-ergoemacs ()
  (setq ergoemacs-theme nil)
  (setq ergoemacs-keyboard-layout "us")
  (require 'ergoemacs-mode)
  (ergoemacs-mode 1))

(feature! ergoemacs
	  "ErgoEmacs"
          (ergoemacs)
          config-ergoemacs
          enable-ergoemacs
          nil)  
