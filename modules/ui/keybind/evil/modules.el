(package! :name evil
	  :docstring "Evil is an extensible vi layer for Emacs. It emulates the main features of Vim, and provides facilities for writing custom extensions. "
	  :pkginfo evil)

(package! :name evil-collection
	  :docstring "This is a collection of Evil bindings for the parts of Emacs that Evil does not cover properly by default"
	  :pkginfo (evil-collection :type git
				    :host github
				    :repo "emacs-evil/evil-collection"))

(package! :name evil-leader
	  :docstring "Provide leader (prefix key) key feature in Emacs
	  evil"
	  :pkginfo (evil-leader :type git
				:host github
				:repo "cofi/evil-leader"))

;;(setq evil-want-keybinding nil)
(defun config-evil ()
  (setq evil-want-keybinding nil)
  t)

(defmacro make-evil (&rest body)
  `(progn
     (evil-mode 1)
     ,@body))

(feature! evil
	  "Evil is an extensible vi layer for Emacs. It emulates the main features of Vim, and provides facilities for writing custom extensions. "
	  (evil undo-tree evil-collection evil-leader)
	  config-evil
	  make-evil
	  nil)
