;;; modules.el --- description

(defun c-c++-after-setup ()
  t)

(scope! c-c++ prog-lang)

(add-hook (scope-function 'c-c++ 'hook :after)
	  #'c-c++-after-setup)

(package! :name google-c-style
	  :docstring "Google C/C++ style"
	  :pkginfo (google-c-style :type git
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

