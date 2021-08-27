;; -*- lexical-binding: t -*-
(defun activate-linumber (scope &optional phase options)
  (pcase scope
    ('modes (let ((status (plist-get options :status)))
	      (if (and status
		       (>= status 0))
		  (progn
		    (linum-mode 1))
		(progn
		  (linum-mode -1)))))
    (_ t)))

(feature! linum
	     "Show line number"
	     nil
	     nil
	     nil
	     activate-linumber)
