(package! :name smartparen-module
	  :docstring "smartparen in buffer"
	  :pkginfo (smartparens :type git
				:host github
				:repo "Fuco1/smartparens"))

(defun enable-smartparens ()
  ;;(backtrace)
  (require 'smartparens)
  (require 'smartparens-config)
  (smartparens-mode 1))

(feature! smartparens
 	  "Enable smart parents"
 	  (smartparen-module)
 	  nil
 	  enable-smartparens
 	  nil)
