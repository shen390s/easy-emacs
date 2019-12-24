(package! :name vimish-fold
	  :docstring "Vim-like text folding for Emacs"
	  :pkginfo vimish-fold)

(defun activate-vimish-fold ()
  (require 'vimish-fold)
  (vimish-fold-mode 1))

(feature! vimish-fold
	  "Vim-like text folding for Emacs"
	  (vimish-fold)
	  nil
	  activate-vimish-fold
	  nil)
