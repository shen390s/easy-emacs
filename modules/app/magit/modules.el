(package-ex! magit
	     "It's Magit! A Git porcelain inside Emacs."
	     magit)

(defun activate-magit (scope &optional phase options)
  t)

(feature-ex! magit
	     "It's Magit! A Git porcelain inside Emacs."
	     (magit)
	     nil
	     nil
	     nil)
