;; -*- lexical-binding: t -*-
(package! rainbow-delimiters
	     "rainbow delimiters"
	     rainbow-delimiters)

(defun activate-rainbow-delimiters (scope &optional phase options)
  (DEBUG! "activate-rainbow-delimiters scope %s phase %s options %s"
	  scope phase options)
  (require 'rainbow-delimiters)

  (let ((status (plist-get options :status)))
    (if (> status 0)
	(rainbow-delimiters-mode 1)
      (rainbow-delimiters-mode -1))))

(feature! rainbow-delimiters
	     "Enable rainbow delimiters"
	     (rainbow-delimiters)
	     nil
	     nil
	     activate-rainbow-delimiters)
