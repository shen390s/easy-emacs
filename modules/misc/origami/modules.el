(package! origami
	     "A text folding minor mode for EMACS"
	     (origami :type git
		      :host github
		      :repo "gregsexton/origami.el"))

(defun origami-evil-keybind()
  (after-activate! evil
		   (progn
		     (require 'evil-leader)
		     (evil-leader/set-key
		       (kbd "oc") 'origami-close-node
		       (kbd "oo") 'origami-open-node
		       (kbd "oac") 'origami-close-all-nodes
		       (kbd "oao") 'origami-open-all-nodes))))

(defun activate-origami (scope &optional phase options)
  (require 'origami)
  (let ((status (plist-get options :status)))
    (if (and status
	     (>= status 0))
	(progn
	  (pcase scope
	    ('app (global-origami-mode 1))
	    ('modes (origami-mode 1))
	    (_ t))
	  (origami-evil-keybind))
      (progn
	(pcase scope
	  ('app (global-origami-mode -1))
	  ('modes (origami-mode -1))
	  (_ t))
	(origami-evil-keybind)))))

(feature! origami
	     "A text folding minor mode for EMACS"
	     (origami)
	     nil
	     nil
	     activate-origami)
