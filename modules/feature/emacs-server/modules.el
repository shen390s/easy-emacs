(defun emacs-server-check ()
  t)

(defun activate-emacs-server ()
  (server-start))

(defun deactivate-emacs-server ()
  (server-force-delete))

(package!
  emacs-server
 "Emacs Editor server"
 nil
 ((emacs-server
   "Emacs Editor Server"
   emacs-server-check
   activate-emacs-server
   deactivate-emacs-server)))

