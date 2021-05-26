(package-ex!  atom-one-dark
	      "Atom One Dark theme for Emacs from Atom.io"
              (atom-one-dark :type git
			     :host github
			     :repo "emacsmirror/atom-one-dark-theme"))

(defun activate-atom-one-dark (scope &optional phase options)
  (pcase scope
    ('ui (let ((status (plist-get options :status)))
	   (when (and status
		      (>= status 0))
	     (load-theme 'atom-one-dark t))))
    (_ t)))

(feature-ex! atom-one-dark
	     "Atom One Dark theme for Emacs"
             (atom-one-dark)
	     nil
	     nil
             activate-atom-one-dark)
