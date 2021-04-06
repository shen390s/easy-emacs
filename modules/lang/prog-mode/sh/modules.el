;;; modules.el --- description

(defun shell-script-after-setup ()
  t)

(add-scope-hook 'shell-script
		'after
		'shell-script-after-setup)
