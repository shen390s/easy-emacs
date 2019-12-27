(package! :name hlinum
	  :docstring "This library extends linum-mode to highlight current line number."
	  :pkginfo (hlinum-mode :type git
				:host github
				:repo "tom-tan/hlinum-mode"))

(defun enable-hlinum ()
  (require 'hlinum)
  (hlinum-activate)
  (DEBUG! "activate hlinum for buffer %s"
	  (buffer-name))
  (linum-mode 1))

(feature! hlinum
	  "This library extends linum-mode to highlight current line number."
	  (hlinum)
	  nil
	  enable-hlinum
	  nil)
