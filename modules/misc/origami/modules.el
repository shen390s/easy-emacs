(package! origami
	  "A text folding minor mode for EMACS"
	  (origami :type git
		   :host github
		   :repo "gregsexton/origami.el"))

(defun origami-key-binds ()
  t)

(defun activate-origami ()
  (require 'origami)
  (origami-key-binds))

(feature! origami
	  "A text folding minor mode for EMACS"
	  (origami)
	  nil
	  activate-origami
	  nil)
