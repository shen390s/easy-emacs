(package! :name rainbow-identifiers
	  :docstring "rainbow identifiers"
	  :pkginfo rainbow-identifiers)

(defun activate-rainbow-identifiers (scope &optional phase options)
  (DEBUG! "activate-rainbow-delimiters scope %s phase %s options %s"
	  scope phase options)
  (require 'rainbow-identifiers)

  (let ((status (plist-get options :status)))
    (if (> status 0)
	(rainbow-identifiers-mode 1)
      (rainbow-identifiers-mode -1))))

(feature-ex! rainbow-identifiers
	  "Enable rainbow identifiers"
	  (rainbow-identifiers)
	  nil
	  nil
	  activate-rainbow-identifiers)
