;; -*- lexical-binding: t -*-
(package!  aanila
	      "A dark emacs theme"
              (aanila :type git
		      :host github
		      :repo "santoshs/aanila"))

(defun activate-aanila (scope &optional phase options)
  (pcase scope
    ('ui (let ((status (plist-get options :status)))
	   (when (and status
		      (>= status 0))
	     (load-theme 'aanila t))))
    (_ t)))

(feature! aanila
	  "A dark emacs theme"
          (aanila)
          nil
	  nil
          activate-aanila)
