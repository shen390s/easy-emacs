;; -*- lexical-binding: t -*-

(defun activate-flymake (scope &optional phase options)
  (DEBUG! "activate-flymake scope %s phase %s options %s"
	  scope phase options)
  (let ((status (plist-get options :status)))
    (if (>= status 0)
	(flymake-mode-on)
      (flymake-mode-off))))

(feature! flymake
	     "flymake"
	     nil
	     nil
	     nil
	     activate-flymake)
