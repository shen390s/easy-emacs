(defun activate-autofill (scope &optional phase options)
  (let ((status (plist-get options
			   :status)))
    (when (and status
	       (>= status 0)))
    (auto-fill-mode 1)))

(feature! auto-fill
	     "Autofill when you edit text"
	     nil
	     nil
	     nil
	     activate-autofill)
