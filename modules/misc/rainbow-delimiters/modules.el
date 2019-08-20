(package! rainbow-delimiters
	  "rainbow delimiters"
	  rainbow-delimiters)


(defun activate-rainbow-delimiters ()
  (require 'rainbow-delimiters)
  (rainbow-delimiters-mode 1))

(feature! rainbow-delimiters
	  "Enable rainbow delimiters"
	  (rainbow-delimiters)
	  nil
	  activate-rainbow-delimiters
	  nil)
