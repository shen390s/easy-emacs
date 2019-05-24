(scope! prog-lang (prog-mode-hook) nil)

(defun prog-lang-after-hook ()
  t)

(add-hook (scope-after-setup-hook 'prog-lang)
	  'prog-lang-after-hook)
