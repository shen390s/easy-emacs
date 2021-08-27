;; -*- lexical-binding: t -*-
(package! boon
	     "An Ergonomic Command mode"
	     (boon :type git
		   :host github
		   :repo "emacsmirror/boon"))

(defun activate-boon (scope &optional phase options)
  (require 'boon-qwerty)
  (pcase scope
    ('app (let ((status (plist-get options :status)))
	    (if (and status
		     (>= status 0))
		(progn
		  (boon-mode 1))
	      (progn
		(boon-mode -1)))))
    (_ t)))

(feature! boon
	     "An Ergonomic command mode"
	     (boon)
	     config-boon
	     nil
	     activate-boon)
