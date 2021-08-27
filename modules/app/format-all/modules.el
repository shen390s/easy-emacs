;; -*- lexical-binding: t -*-
(package!  format-all
	      "Auto-format c,c++,JS,Python and more"
              (format-all :type git
			  :host github
			  :repo "emacsmirror/format-all"))

(defun activate-format-all (scope &optional phase options)
  (require 'format-all)

  (pcase scope
    ('modes (progn
	      (let ((status (plist-get options :status)))
		(if (and status
			 (>= status 0))
		    (format-all-mode 1)
		  (format-all-mode -1)))))
    (_ t)))
  
(feature! format-all
	     "Auto-format c,c++,JS,Python and more"
             (format-all)
             nil
	     nil
             activate-format-all)
