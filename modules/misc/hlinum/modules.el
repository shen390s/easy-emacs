(package!
 hlinum
 "This library extends linum-mode to highlight current line number."
 (hlinum-mode :type git
	      :host github
	      :repo "tom-tan/hlinum-mode"))

(defun enable-hlinum ()
  (require 'hlinum)
  (hlinum-activate)
  (linum-mode 1))

(feature!
 hlinum
 "This library extends linum-mode to highlight current line number."
 (hlinum)
 nil
 enable-hlinum
 nil)
