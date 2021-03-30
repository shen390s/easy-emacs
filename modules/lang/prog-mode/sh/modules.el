;;; modules.el --- description

(defun shell-script-after-setup ()
  t)

(scope! shell-script prog-lang)

(add-scope-hook 'shell-script
		'after
		'shell-script-after-setup)
