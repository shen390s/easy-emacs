(package! :name hlinum
	  :docstring "This library extends linum-mode to highlight current line number."
	  :pkginfo (hlinum-mode :type git
				:host github
				:repo "tom-tan/hlinum-mode"))

(defun activate-hlinum (scope &optional phase options)
  (require 'hlinum)
  (hlinum-activate)
  (DEBUG! "activate hlinum for buffer %s"
	  (buffer-name))
  (linum-mode 1))

(feature-ex! hlinum
	     "This library extends linum-mode to highlight current line number."
	     (hlinum)
	     nil
	     nil
	     activate-hlinum
	     nil)
