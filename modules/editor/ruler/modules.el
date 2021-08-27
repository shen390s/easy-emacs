;; -*- lexical-binding: t -*-
(defun activate-ruler (scope &optional phase options)
  (DEBUG! "activate-ruler scope %s phase %s options %s"
	  scope phase  options)
  (let ((status (plist-get options :status)))
    (if (> status 0)
	(ruler-mode 1)
      (ruler-mode -1))))

(feature! ruler
	  "Display ruler when editing"
	  nil
	  nil
	  nil
	  activate-ruler)
