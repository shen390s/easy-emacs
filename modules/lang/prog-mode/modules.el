(scope! prog-lang nil prog-mode)

(defun prog-lang-after-hook ()
  t)

(add-hook (scope-function 'prog-lang 'hook :after)
	  'prog-lang-after-hook)
