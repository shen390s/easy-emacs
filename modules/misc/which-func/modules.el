
(defun activate-which-func (scope &optional phase options)
  (pcase scope
    ('modes (let ((status (plist-get options :status)))
	      (if (and status
		       (>= status 0))
		  (progn
		    (which-function-mode 1))
		(progn
		  (which-function-mode -1)))))
    (_ t)))

(feature! which-func
	     "Show the name of current function"
	     nil
	     nil
	     nil
	     activate-which-func)
