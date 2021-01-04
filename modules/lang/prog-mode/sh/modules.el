;;; modules.el --- description

(defun shell-script-after-setup ()
  t)

(scope! shell-script prog-lang)

(add-hook (scope-function 'shell-script 'hook :after)
	  #'shell-script-after-setup)
