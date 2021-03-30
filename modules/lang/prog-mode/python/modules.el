(defun python-after-setup ()
  t)

(scope! python prog-lang)

(add-scope-hook 'python
		'after
		'python-after-setup)

