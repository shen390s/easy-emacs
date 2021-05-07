(package-ex! smartparen-module
	     "smartparen in buffer"
	     (smartparens :type git
			  :host github
			  :repo "Fuco1/smartparens"))

(defun activate-smartparens (scope &optional phase options)
  ;;(backtrace)
  (require 'smartparens)
  (require 'smartparens-config)
  (if (> (plist-get options :status) 0)
      (smartparens-mode 1)
    (smartparens-mode -1)))

(feature-ex! smartparens
 	     "Enable smart parents"
 	     (smartparen-module)
 	     nil
	     nil
	     activate-smartparens)
