(defun python-after-setup ()
  t)

(add-scope-hook 'python
		'after
		'python-after-setup)

