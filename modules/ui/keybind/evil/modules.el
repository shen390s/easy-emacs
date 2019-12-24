(package! :name evil
	  :docstring "Evil is an extensible vi layer for Emacs. It emulates the main features of Vim, and provides facilities for writing custom extensions. "
	  :pkginfo evil)

(package! :name evil-collection
	  :docstring "This is a collection of Evil bindings for the parts of Emacs that Evil does not cover properly by default"
	  :pkginfo (evil-collection :type git
				    :host github
				    :repo "emacs-evil/evil-collection"))

;;(setq evil-want-keybinding nil)

(defun make-evil ()
  ;;(setq evil-want-integration t)
  ;;(setq evil-want-keybinding nil)
  (evil-mode 1)
  ;;(evil-collection-init)
  )

(feature! evil
	  "Evil is an extensible vi layer for Emacs. It emulates the main features of Vim, and provides facilities for writing custom extensions. "
	  (evil undo-tree evil-collection)
	  nil
	  make-evil
	  nil)
