(defun enable-hideshow ()
  (require 'hideshow)
  (hs-minor-mode 1))

(defun deactive-hidehsow ()
  (turn-off-hideshow))

(feature! hideshow
	  "show/hide blocks"
          nil
          nil
          enable-hideshow
          deactivate-hideshow)
