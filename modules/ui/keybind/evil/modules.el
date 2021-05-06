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
(defun config-evil (scope &optional phase options)
  (setq evil-want-keybinding nil)
  t)

(defmacro turn-on-evil (scope &optional phase options)
  `(progn
     (DEBUG! "turn-on-evil scope %s phase %s options %s"
	     scope phase options)
     (require 'evil)
     (require 'evil-leader)
     (evil-mode 1)
     (global-evil-leader-mode)
     ,@(plist-get options :after-activate)))

(defun activate-evil (scope &optional phase options)
  (DEBUG! "activate-evil scope %s phase %s options %s"
	  scope phase options)
  (turn-on-evil `,scope `,phase `,options))

(feature-ex! evil
	     "Evil is an extensible vi layer for Emacs. It emulates the main features of Vim, and provides facilities for writing custom extensions. "
	     (evil undo-tree evil-collection evil-leader)
	     config-evil
	     nil
	     activate-evil)
