(feature! eldoc
	  "eldoc"
	  nil
	  nil
	  nil
	  nil)

(defun eldoc-enabled ()
  (let ((enabled-scopes (feature-enabled 'eldoc)))
    (if (member current-scope enabled-scopes)
	t
      (member 'global enabled-scopes))))
