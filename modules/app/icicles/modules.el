(package! icicles
	  "Minibuffer input completion and cycling"
	  (icicles :type git
		   :host github
		   :repo "emacsmirror/icicles"))


(defun activate-icicles ()
  (require 'icicles)
  (icy-mode 1))

(feature! icicles
	  "minibuffer input completion and cycling"
	  (icicles)
	  nil
	  activate-icicles
	  nil)
