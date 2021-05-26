(package! golden-ratio
	     "Automatic resizing of Emacs windows to the golden ratio "
	     (golden-ratio :type git
			   :host github
			   :repo "roman/golden-ratio.el"))

(defun activate-golden-ratio (scope &optional phase options)
  (require 'golden-ratio)
  (pcase scope
    ('app (let ((status (plist-get options :status)))
	    (if (and status
		     (>= status 0))
		(progn
		  (golden-ratio-mode 1))
	      (progn
		(golden-ratio-mode -1)))))
    (_ t)))

(feature! golden-ratio
	     "roman/golden-ratio.el"
	     (golden-ratio)
	     nil
	     nil
	     activate-golden-ratio)
