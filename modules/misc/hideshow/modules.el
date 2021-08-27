;; -*- lexical-binding: t -*-
(defun activate-hideshow (scope &optional phase options)
  (require 'hideshow)
  (pcase scope
    ('modes (let ((status (plist-get options :status)))
	      (if (and status
		       (>= status 0))
		  (progn 
		    (hs-minor-mode 1))
		(progn
		  (turn-off-hideshow)))))
    (_ t)))

(feature! hideshow
	     "show/hide blocks"
             nil
             nil
	     nil
             activate-hideshow)
