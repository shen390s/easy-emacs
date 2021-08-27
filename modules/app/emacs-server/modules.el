;; -*- lexical-binding: t -*-

(defun emacs-server-config/:pre-check (scope &optional options)
  (DEBUG! "configuring emacs server options %s"
	  options)
  (let ((port (car (plist-get options :port)))
	(auth-key (car (plist-get options :auth-key))))
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
    (:pre-check (emacs-server-config/:pre-check scope options))
    (_ t)))

(defun emacs-server/:start (scope &optional options)
  (DEBUG! "starting emacs server options %s"
	  options)
  (server-start))


(defun activate-emacs-server (scope &optional phase options)
  (DEBUG! "activate-emacs-server scope %s phase %s options"
	  scope phase options)
  (emacs-server/:start scope options))

(feature! emacs-server
	     "Emacs Editor Server"
	     nil
	     emacs-server-config
	     nil
	     activate-emacs-server)

