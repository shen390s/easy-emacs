(package! smartparen-module
	  "smartparen in buffer"
	  (smartparens :type git
		       :host github
		       :repo "Fuco1/smartparens"))

(defun enable-smartparens ()
  ;;(backtrace)
  (require 'smartparens)
  (smartparens-mode))

(feature! smartparens
 	  "Enable smart parents"
 	  (smartparen-module)
 	  nil
 	  enable-smartparens
 	  nil)

