(package! modalka
	     "Easily introduce native modal editing of your own design"
	     (modalka :type git
		      :host github
		      :repo "mrkkrp/modalka"))

(defun activate-modalka (scope &optional phase options)
  (require 'modalka)
  (pcase scope
    ('app (let ((status (plist-get options :status)))
	    (if (and status
		     (>= status 0))
		(progn
		  (modalka-mode 1))
	      (progn
		(modalka-mode -1)))))
    (_ t)))

(feature! modalka
	     "Easily introduce native modal editing of your own design"
	     (modalka)
	     nil
	     nil
	     activate-modalka)
