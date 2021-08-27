;; -*- lexical-binding: t -*-
(defvar the-theme nil
  "The theme which activated by color-theme")

(defun activate-color-theme (scope &optional phase options)
  (pcase scope
    ('ui (let ((status (plist-get options :status)))
	   (when (and status
		      (>= status 0))
	     (load-theme the-theme t))))
    (_ t)))
  
(feature! color-theme
	     "choose color theme"
             nil
	     nil
	     nil
             activate-color-theme)
