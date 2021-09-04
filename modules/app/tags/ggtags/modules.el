;; -*- lexical-binding: t -*-
(package! ggtags
	     "Emacs frontend to GNU Global source code tagging system. "
	     ggtags)

(defun config-ggtags (scope &optional phase options)
  (DEBUG! "config-ggtags scope %s phase %s options %s"
	  scope phase options)
  t)

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
