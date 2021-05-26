;; -*- lexical-binding: t -*-
(package! ggtags
	     "Emacs frontend to GNU Global source code tagging system. "
	     ggtags)

(defun config-ggtags (scope &optional phase options)
  (DEBUG! "config-ggtags scope %s phase %s options %s"
	  scope phase options)
  (pcase phase
    (:check (after-activate!
	     evil
	     (progn
	       (DEBUG! "configure ggtags when evil on scope %s options %s"
		       scope options)
	       (require 'evil-leader)
	       (evil-leader/set-key-for-mode (get-mode-from-options options)
		 (kbd "mgd") 'ggtags-find-definition
		 (kbd "mgr") 'ggtags-find-reference
		 (kbd "mgtr") 'ggtags-find-tag-regexp
		 (kbd "mgos") 'ggtags-find-other-symbol
		 (kbd "mgf") 'ggtags-find-file))))
    (_ t)))

(defun activate-ggtags (scope &optional phase options)
  (require 'ggtags)
  (let ((status (plist-get options :status)))
    (when status
      (if (>= status 0)
	  (progn
	    (ggtags-mode 1))
	(progn
	  (ggtags-mode -1))))))

(feature! ggtags
	     "Emacs frontend to GNU Global source code tagging system. "
	     (ggtags)
	     config-ggtags
	     nil
	     activate-ggtags)
