(defun activate-autofill ()
  (auto-fill-mode 1))

(feature! auto-fill
	  "Autofill when you edit text"
	  nil
	  nil
	  activate-autofill
	  nil)
