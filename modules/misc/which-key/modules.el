(package! which-key
	     "which-key is a minor mode for Emacs that displays the 
key bindings following your currently entered incomplete command (a prefix) 
in a popup"
	     which-key)

(defun activate-which-key (scope &optional phase options)
  (DEBUG! "activate-which-key scope %s phase %s options %s"
	  scope phase options)
  (require 'which-key)
  (let ((status (plist-get options :status)))
    (when status
      (if (>= status 0)
	  (progn
	    (which-key-mode 1))
	(which-key-mode -1)))))

(feature! which-key
	     "which-key is a minor mode for Emacs that displays the 
key bindings following your currently entered incomplete command (a prefix) 
in a popup"
	     (which-key)
	     nil
	     nil
	     activate-which-key)
