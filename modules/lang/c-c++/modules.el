;;; modules.el --- description

(scope! c-c++ (c-mode-hook c++-mode-hook) nil)

(package!
 google-c-style
 "Google C/C++ style"
 (google-c-style :type git :host github :repo "emacsmirror/google-c-style"))

(defun enable-google-c-style ()
  (google-set-c-style))

(feature!
 google-c-style
 "Google C/C++ style"
 (google-c-style)
 nil
 enable-google-c-style
 nil)


