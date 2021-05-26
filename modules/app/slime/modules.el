(package! slime
	     "SLIME is the Superior Lisp Interaction Mode for Emacs."
	     slime)

(defun activate-slime (scope &optional phase options)
  (DEBUG! "activate-slime scope %s phase %s options %s"
	  scope phase options)
  (let ((fancy-status (plist-get options :slime-fancy)))
    (when fancy-status
      (when (>= fancy status 0)
	(setq slime-contribs '(slime-fancy))))))

(feature! slime
	     "SLIME is the Superior Lisp Interaction Mode for Emacs."
	     (slime)
	     nil
	     nil
	     activate-slime)
