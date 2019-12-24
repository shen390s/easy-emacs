(package! :name rainbow-delimiters
	  :docstring "rainbow delimiters"
	  :pkginfo rainbow-delimiters)


(defun activate-rainbow-delimiters ()
  (require 'rainbow-delimiters)
  (rainbow-delimiters-mode 1))

(feature! rainbow-delimiters
	  "Enable rainbow delimiters"
	  (rainbow-delimiters)
	  nil
	  activate-rainbow-delimiters
	  nil)
