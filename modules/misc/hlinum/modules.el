(package-ex! hlinum
	  "This library extends linum-mode to highlight 
current line number."
	  (hlinum-mode :type git
		       :host github
		       :repo "tom-tan/hlinum-mode"))

(defun activate-hlinum (scope &optional phase options)
  (DEBUG! "activate-hlinum scope %s phase %s options %s"
	  scope phase options)
  (require 'hlinum)
  (let ((status (plist-get options :status)))
    (if (> status 0)
	(progn
	  (hlinum-activate)
	  (linum-mode 1))
      (progn
	(hlinum-deactivate)
	(linum-mode -1)))))

(feature-ex! hlinum
	     "This library extends linum-mode to highlight current line number."
	     (hlinum)
	     nil
	     nil
	     activate-hlinum)
