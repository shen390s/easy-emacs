(defun python-after-setup ()
  t)

(scope! python
	python-mode)

(add-hook (scope-function 'python 'hook :after)
	  'python-after-setup)
