(package! :name undo-tree
	  :docstring "undo-tree-mode treats undo history as a branching tree of changes, similar to the way Vim handles it. This makes it substantially easier to undo and redo any change, while preserving the entire history of past states. "
	  :pkginfo undo-tree)

(defun enable-undo-tree ()
  (global-undo-tree-mode))

(feature!
 undo-tree
 "undo-tree-mode treats undo history as a branching tree of changes, similar to the way Vim handles it. This makes it substantially easier to undo and redo any change, while preserving the entire history of past states. "
 (undo-tree)
 nil
 enable-undo-tree
 nil)
