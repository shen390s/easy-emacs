
(defun emacs-server-config/:pre-check (scope &optional options)
  (DEBUG! "configuring emacs server options %s"
	  options)
  (let ((port (plist-get options :port))
	(auth-key (plist-get options :auth-key)))
    (if port
	(setq server-port port)
      (setq server-port (getenv "EMACS_SERVER_PORT")))
    (if auth-key
	(setq server-auth-key auth-key)
      (setq server-auth-key (getenv "EMACS_SERVER_AUTH")))
    (when (and server-port
	       server-auth-key)
      (setq server-use-tcp t
	    server-host "127.0.0.1"))))

(defun emacs-server-config (scope &optional phase options)
  (DEBUG! "emacs server configure phase %s" phase)
  (pcase phase
    ("pre-check" (emacs-server-config/:pre-check scope options))
    (_ t)))

(defun emacs-server-pkgs (scope options)
  (DEBUG! "update pkg list of emacs-server with option %s"
	  options)
  nil)

(defun emacs-server-activate/:after (scope &optional options)
  (DEBUG! "starting emacs server options %s"
	  options)
  (server-start))

(defun activate-emacs-server (scope &optional phase options)
  (pcase phase
    ("after-activate" (emacs-server-activate/:after scope options))
    (_ t)))

(defun deactivate-emacs-server ()
  (server-force-delete))

(feature! emacs-server
	  "Emacs Editor Server"
	  emacs-server-pkgs
	  emacs-server-config
	  activate-emacs-server
	  deactivate-emacs-server)

