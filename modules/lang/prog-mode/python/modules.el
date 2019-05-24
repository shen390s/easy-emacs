(defun python-after-setup ()
  (unless (member 'python
		  (feature-enabled 'flymake))
    (flymake-mode 0))
  (unless (eldoc-enabled)
    (message "disbling eldoc...")
    (global-eldoc-mode -1)
    (eldoc-mode -1)))

(scope! python
	(python-mode-hook)
	nil)

(add-hook (scope-after-setup-hook 'python)
	  'python-after-setup)
