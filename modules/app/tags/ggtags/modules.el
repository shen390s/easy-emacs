(package! :name ggtags
	  :docstring "Emacs frontend to GNU Global source code tagging system. "
	  :pkginfo ggtags)

(defun config-ggtags ()
  t)

(defun enable-ggtags ()
  (require 'ggtags)
  (ggtags-mode 1))

(feature! ggtags
	  "Emacs frontend to GNU Global source code tagging system. "
	  (ggtags)
	  config-ggtags
	  enable-ggtags
	  nil)
