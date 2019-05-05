(defun emacs-server-check ()
  t)

(defun activate-emacs-server ()
  (server-start))

(defun deactivate-emacs-server ()
  (server-force-delete))

(feature!
 emacs-server
 "Emacs Editor Server"
 nil
 emacs-server-check
 activate-emacs-server
 deactivate-emacs-server)

