(defun emacs-server-check ()
  t)

(defun activate-emacs-server ()
  (server-start))

(defun deactivate-emacs-server ()
  (server-force-delete))

(module!
 emacs-server
 "Emacs Editor server"
 nil
 ((emacs-server
   #'emacs-server-check
   #'activate-emacs-server
   #'deactivate-emacs-server)))
   
