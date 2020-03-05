(scope! fish prog-lang)

(package! :name fish-mode
	  :docstring "A major Emacs mode for edit Fish source code"
	  :pkginfo (fish-mode :type git
			      :host github
			      :repo "wwwjfy/emacs-fish"))

(defun config-fish ()
  (add-to-list 'auto-mode-alist
	       '("\\.fish\\'" . lang/fish-mode))
  t)

(autoload-r! fish-mode
	     (fish-mode)
	     "fish-mode"
	     t)

(rmode! lang/fish-mode
	"Emacs mode for fish program language"
	(fish-mode)
	config-fish
	fish-mode)

