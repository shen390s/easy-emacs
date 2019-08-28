(defun activate-ruler ()
  (ruler-mode 1))

(feature! ruler
	  "Display ruler when editing"
	  nil
	  nil
	  activate-ruler
	  nil)
