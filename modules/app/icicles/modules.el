(package! :name icicles
	  :docstring "Minibuffer input completion and cycling"
	  :pkginfo (icicles :type git
			    :host github
			    :repo "emacsmirror/icicles"))


(defun activate-icicles ()
  (condition-case err
      ((require 'icicles)
       (icy-mode 1))
    (error (INFO! "active icciles failed: %s"
                  (error-message-string err))
                  nil)))

(feature! icicles
	  "minibuffer input completion and cycling"
	  (icicles)
	  nil
	  activate-icicles
	  nil)
