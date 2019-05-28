(defun python-after-setup ()
  t)

(scope! python
	prog-lang
	python-mode)

(add-hook (scope-function 'python 'hook :after)
	  'python-after-setup)
