;;; modules.el --- description

(package-ex! google-c-style
	     "Google C/C++ style"
	     (google-c-style :type git
			     :host github
			     :repo "emacsmirror/google-c-style"))

(defun activate-google-c-style (scope &optional phase options)
  (let ((status (plist-get options :status)))
    (when status
      (when (>= status 0)
	(google-set-c-style)))))

(feature-ex! google-c-style
	     "Google C/C++ style"
	     (google-c-style)
	     nil
	     nil
	     activate-google-c-style)


