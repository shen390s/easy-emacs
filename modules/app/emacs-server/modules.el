(defvar emacs-server-host-port 3861
  "The tcp port which emacs server will listen")
(defvar emacs-server-auth-key "killUS.DOG.2020"
  "auth key for emacs connection")

(defun emacs-server-check ()
  (progn
    (setq server-use-tcp t
	  server-host (system-name)
	  server-port emacs-server-host-port
	  server-auth-key emacs-server-auth-key)
    t))

(defun activate-emacs-server ()
  (server-start))

(defun deactivate-emacs-server ()
  (server-force-delete))

(feature! emacs-server
	  "Emacs Editor Server"
	  nil
	  emacs-server-check
	  activate-emacs-server
	  deactivate-emacs-server)

