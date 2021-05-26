(package-ex! evil
	  "Evil is an extensible vi layer for Emacs. 
It emulates the main features of Vim, and provides 
facilities for writing custom extensions. "
	  evil)

(package-ex! evil-collection
	  "This is a collection of Evil bindings for 
the parts of Emacs that Evil does not cover properly 
by default"
	  (evil-collection :type git
			   :host github
			   :repo "emacs-evil/evil-collection"))

(package-ex! evil-leader
	  "Provide leader (prefix key) key feature in Emacs
evil"
	  (evil-leader :type git
				:host github
				:repo "cofi/evil-leader"))

;;(setq evil-want-keybinding nil)
(defun config-evil (scope &optional phase options)
  (setq evil-want-keybinding nil)
  t)

(defun activate-evil (scope &optional phase options)
  (DEBUG! "activate-evil scope %s phase %s options %s"
	  scope phase options)
  (require 'evil)
  (require 'evil-leader)
  (global-evil-leader-mode)
  (evil-mode 1))

(feature-ex! evil
	     "Evil is an extensible vi layer for Emacs. 
It emulates the main features of Vim, 
and provides facilities for writing custom extensions. "
	     (evil undo-tree evil-collection evil-leader)
	     config-evil
	     nil
	     activate-evil)
