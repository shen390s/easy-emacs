(defun prog-lang-after-hook ()
  t)


(add-scope-hook 'prog-lang
		'after
		'prog-lang-after-hook)
