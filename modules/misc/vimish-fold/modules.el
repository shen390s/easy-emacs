(package! vimish-fold
	  "Vim-like text folding for Emacs"
	  vimish-fold)

(defun activate-vimish-fold ()
  (require 'vimish-fold)
  (vimish-fold-mode 1))

(feature! vimish-fold
	  "Vim-like text folding for Emacs"
	  (vimish-fold)
	  nil
	  activate-vimish-fold
	  nil)
