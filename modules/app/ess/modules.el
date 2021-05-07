(package-ex! ess
	     "Emacs Speaks Statistics"
             ess)

(defun activate-ess (scope &optional phase options)
  (when (and (executable-find "R")
	     (> (plist-get options :status) 0))
    (load "ess-autoloads")))
  
(feature-ex! ess
	     "Emacs Speaks Statistics"
             (ess)
             nil
	     nil
             activate-ess)
