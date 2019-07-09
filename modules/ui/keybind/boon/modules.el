(package! boon
	  "An Ergonomic Command mode"
	  (boon :type git
		:host github
		:repo "emacsmirror/boon"))

(defun config-boon ()
  t)

(defun enable-boon ()
  (require 'boon-qwerty)
  (boon-mode))

(feature! boon
	  "An Ergonomic command mode"
	  (boon)
	  config-boon
	  enable-boon
	  nil)
