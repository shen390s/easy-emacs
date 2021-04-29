
(defun emacs-server-config (scope-name phase options)
  (progn
    (setq server-port (getenv "EMACS_SERVER_PORT")
	  server-auth-key (getenv "EMACS_SERVER_AUTH"))
    (when (and server-port
	       server-auth-key)
      (setq server-use-tcp t
	    server-host "127.0.0.1"))
    t))

(defun emacs-server-pkgs (scope-name options)
  (DEBUG! "update pkg list of emacs-server with option %s"
	  options)
  nil)

(defun activate-emacs-server ()
  (server-start))

(defun deactivate-emacs-server ()
  (server-force-delete))

(feature! emacs-server
	  "Emacs Editor Server"
	  emacs-server-pkgs
	  emacs-server-config
	  activate-emacs-server
	  deactivate-emacs-server)

