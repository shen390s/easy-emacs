(defun eldoc (global status)
  (if global
      (global-eldoc-mode status)
    (eldoc-mode status)))

(defun activate-eldoc (scope &optional phase options)
  (DEBUG! "activate-eldoc scope %s phase %s options %s"
	  scope phase options)
  (if (>= (plist-get options :status) 0)
      (eldoc (eq scope 'editor) 1)
    (eldoc (eq scope 'editor) -1)))

(feature-ex! eldoc
	     "eldoc"
	     nil
	     nil
	     nil
	     activate-eldoc)

