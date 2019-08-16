(package! modalka
	  "Easily introduce native modal editing of your own design"
	  (modalka :type git
		   :host github
		   :repo "mrkkrp/modalka"))

(defun activate-modalka ()
  (require 'modalka)
  (modalka-mode 1))

(feature! modalka
	  "Easily introduce native modal editing of your own design"
	  (modalka)
	  nil
	  activate-modalka
	  nil)
