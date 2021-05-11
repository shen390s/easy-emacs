(defvar lang-server-client-features nil
  "lang server client")

(defvar lang-server-c-server nil
  "lang server for c/c++")

(defun setup-lang-server-features (options)
  (DEBUG! "setup-lang-server-features options %s"
	  options)
  (if (feature-on 'eglot options)
      (push 'eglot lang-server-client-features)
    (push 'lsp lang-server-client-features))
  (if (feature-on 'cquery options)
      (push 'cquery lang-server-c-server)
    (if (feature-on 'ccls options)
	(push 'ccls lang-server-c-server)
      (if (feature-on 'clangd options)
	  (push 'clangd lang-server-c-server)
	(push 'cquery lang-server-c-server)))))

(defun lang-server-pkgs (scope &optional options)
  (DEBUG! "lang-server-pkgs scope %s options %s"
	  scope options)
  (let ((status (plist-get options :status)))
    (if (and status
	     (< status 0))
	nil
      (pcase scope
	('app
	 (packages (append lang-server-client-features
			   lang-server-c-server)))
	('modes t)
	(_ t)))))

(defun lang-server-config/:pre-check (scope &optional phase options)
  (pcase scope
    ('app (setup-lang-server-features options))
    ('modes t)
    (_ t)))

(defun lang-server-config (scope &optional phase options)
  (DEBUG! "lang-server-config scope %s phase %s options %s"
	  scope phase options)
  (pcase phase
    (:pre-check (lang-server-config/:pre-check scope phase options))
    (_ t)))

(defun lang-server-prepare (scope &optional phase options)
  (pcase phase
    (_ t)))

(defun lang-server-activate (scope &optional phase options)
  (pcase phase
    (_ t)))

(feature-ex! lang-server
	     "Emacs language server"
	     lang-server-pkgs
	     lang-server-config
	     lang-server-prepare
	     lang-server-activate)

