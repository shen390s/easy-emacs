;; -*- lexical-binding: t -*-
(package!  telephone-line
	      "Telephone Line is a new implementation of Powerline for emacs"
	      (telephone-line :type git
			 :host github
			 :repo "dbordak/telephone-line"))

(defun activate-telphone-line (scope &optional phase options)
  (require 'telephone-line)
  (let ((status (plist-get options :status)))
    (when (and status
	       (>= status 0))
      (telephone-line-mode 1))))

(feature! telephone-line
	     "Telephone Line is a new implementation of Powerline for emacs"
	     (telephone-line)
	     nil
	     nil
	     activate-telphone-line)
	     
