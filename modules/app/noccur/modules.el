(package! noccur
	     "Run multi-occur on project/dired files"
	     (noccur :type git
		     :host github
		     :repo "NicolasPetton/noccur.el"))

(defun config-noccur (scope &optional phase options)
  t)

(defun activate-noccur (scope &optional phase options)
  (pcase scope
    ('app (let ((status (plist-get options :status)))
	    (when (and status
		       (>= status 0))
	      (require 'noccur))))
    (_ t)))

(feature! noccur
	     "NicolasPetton/noccur.el"
	     (noccur)
	     config-noccur
	     nil
	     activate-noccur)
