;;; modules.el --- description

(defun c-c++-after-setup ()
  ;; disable flymake if we have not turned
  ;; on explicit
  (unless (member 'c-c++
		  (feature-enabled 'flymake))
    (flymake-mode 0))
  (unless (member 'c-c++
		  (feature-enabled 'eldoc))
    (eldoc-mode -1)))

(scope! c-c++
	(c-mode-hook c++-mode-hook)
	nil
	(c-c++-after-setup))

(package! google-c-style
	  "Google C/C++ style"
	  (google-c-style :type git
			  :host github
			  :repo "emacsmirror/google-c-style"))

(defun enable-google-c-style ()
  (google-set-c-style))

(feature! google-c-style
	  "Google C/C++ style"
	  (google-c-style)
	  nil
	  enable-google-c-style
	  nil)

(feature! set-c-style
	  "set c/c++ style"
	  nil
	  nil
	  nil
	  nil)
