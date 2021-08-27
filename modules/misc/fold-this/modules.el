;; -*- lexical-binding: t -*-
(package! fold-this
	     "Fold the active region"
             (fold-this :type git
			:host github
			:repo "magnars/fold-this.el"))

(defun activate-fold-this (scope &optional phase options)
  (require 'fold-this)
  (pcase scope
    ('modes (progn
	      (let ((status (plist-get options :status)))
		(if (and status
			 (>= status 0))
		    (progn
		      (fold-this-mode 1))
		  (progn
		    (fold-this-mode -1))))))
    (_ t)))

(feature! fold-this
	     "fold the actived region"
             (fold-this)
             nil
	     nil
             activate-fold-this)

