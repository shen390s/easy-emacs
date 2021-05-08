(package-ex! undo-tree
	     "undo-tree-mode treats undo history as a branching tree
	     of changes, similar to the way Vim handles it. This makes
	     it substantially easier to undo and redo any change,
	     while preserving the entire history of past states. "
	     undo-tree)

(defun activate-undo-tree (scope &optional phase options)
  (DEBUG! "activate-undo-tree scope %s phase %s options %s"
	  scope phase options)
  (require 'undo-tree)
  (when (eq scope 'editor)
    (if (> (plist-get options :status) 0)
	(global-undo-tree-mode 1)
      (global-undo-tree-mode -1))))

(feature-ex! undo-tree
	     "undo-tree-mode treats undo history as a branching tree
	     of changes, similar to the way Vim handles it. This makes
	     it substantially easier to undo and redo any change, while
	     preserving the entire history of past states. " 
	     (undo-tree)
	     nil
	     nil
	     activate-undo-tree)
