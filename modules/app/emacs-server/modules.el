
(defun emacs-server-config ()
  (progn
    (setq server-port (getenv "EMACS_SERVER_PORT")
	  server-auth-key (getenv "EMACS_SERVER_AUTH"))
    (when (and server-port
	       server-auth-key)
      (setq server-use-tcp t
	    server-host (system-name)))
    t))

(defun activate-emacs-server ()
  (server-start))

(defun deactivate-emacs-server ()
  (server-force-delete))

(feature! emacs-server
	  "Emacs Editor Server"
	  nil
	  emacs-server-config
	  activate-emacs-server
	  deactivate-emacs-server)

