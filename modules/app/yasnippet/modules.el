(package-ex! yasnippet
	     "YASnippet is a template system for Emacs"
	     (yasnippet :type git
			:host github
			:repo "joaotavora/yasnippet"))

(defun activate-yasnippet (scope &optional phase options)
  (require 'yasnippet)
  (when (eq scope 'editor)
    (let ((status (plist-get options :status)))
      (when (and status
		 (>= status 0))
	(yas-global-mode 1)))))

(feature-ex! yasnippet
	     "YASnippet is a template system for Emacs"
	     (yasnippet)
	     nil
	     nil
	     activate-yasnippet)
