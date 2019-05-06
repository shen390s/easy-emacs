(package!
 evil
 "Evil is an extensible vi layer for Emacs. It emulates the main features of Vim, and provides facilities for writing custom extensions. "
 evil)

(package!
 evil-collection
 "This is a collection of Evil bindings for the parts of Emacs that Evil does not cover properly by default"
 (evil-collection :type git :host github :repo "emacs-evil/evil-collection"))

(setq evil-want-keybinding nil)

(defun make-evil ()
  (evil-mode 1)
  (evil-collection-init))

(feature!
 evil
 "Evil is an extensible vi layer for Emacs. It emulates the main features of Vim, and provides facilities for writing custom extensions. "
 (evil undo-tree evil-collection)
 nil
 make-evil
 nil)
