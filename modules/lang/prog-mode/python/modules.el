(defun python-after-setup ()
  t)

(scope! python
	prog-lang)

(add-hook (scope-function 'python 'hook :after)
	  'python-after-setup)
