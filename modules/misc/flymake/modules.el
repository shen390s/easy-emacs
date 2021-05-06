
(defun activate-flymake (scope &optional phase options)
  (DEBUG! "activate-flymake scope %s phase %s options %s"
	  scope phase options)
  (let ((status (plist-get options :status)))
    (if (> status 0)
	(flymake-mode 1)
      (flymake-mode -1))))

(feature-ex! flymake
	     "flymake"
	     nil
	     nil
	     nil
	     activate-flymake)
