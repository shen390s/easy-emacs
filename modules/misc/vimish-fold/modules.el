;; -*- lexical-binding: t -*-
(package! vimish-fold
	     "Vim-like text folding for Emacs"
	     vimish-fold)

(defun activate-vimish-fold (scope &optional phase options)
  (require 'vimish-fold)
  (pcase scope
    ('modes (let ((status (plist-get options :status)))
	      (if (and status
		       (>= status 0))
		  (progn
		    (vimish-fold-mode 1))
		(progn
		  (vimish-fold-mode -1)))))
    (_ t)))

(feature! vimish-fold
	  "Vim-like text folding for Emacs"
	  (vimish-fold)
	  nil
	  nil
	  activate-vimish-fold)
