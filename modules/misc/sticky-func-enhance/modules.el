(package! semantic-stickyfunc-enhance
	     "Improved version of semantic-stickyfunc-mode that handles parameters on multiple lines "
	     (semantic-stickyfunc-enhance :type git
					  :host github
					  :repo "tuhdo/semantic-stickyfunc-enhance"))

(defun activate-stickyfunc-enhance (scope &optional phase options)
  (require 'stickyfunc-enhance)
  (pcase scope
    ('modes (progn
	      (let ((status (plist-get options :status)))
		(if (and status
			 (>= status 0))
		    (progn
		      (semantic-mode 1)
		      (semantic-stickyfunc-mode 1))
		  (semantic-stickyfunc-mode -1)))))
    (_ t)))

(feature! stickyfunc-enhance
	     "Improved version of semantic-stickyfunc-mode that handles parameters on multiple lines"
	     (semantic-stickyfunc-enhance)
	     nil
	     nil
	     activate-stickyfunc-enhance)
