(package! :name rainbow-identifiers
	  :docstring "rainbow identifiers"
	  :pkginfo rainbow-identifiers)

(defun activate-rainbow-identifiers ()
  (require 'rainbow-identifiers)
  (rainbow-identifiers-mode 1))

(feature! rainbow-identifiers
	  "Enable rainbow identifiers"
	  (rainbow-identifiers)
	  nil
	  activate-rainbow-identifiers
	  nil)
