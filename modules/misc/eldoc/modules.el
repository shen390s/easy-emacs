(defvar eldoc-pkgs-config
  (list :c '(c-eldoc)
	:prog '(c-eldoc))
  "Configuration of eldoc for modes")

(package-ex! c-eldoc
	     "Display description of the function under the cursor."
	     (c-eldoc :type git
		      :host github
		      :repo "nflath/c-eldoc"))

(defun eldoc-config (scope &optional phase options)
  t)

(defun eldoc-prepare (scope &optional phase options)
  t)

(defun eldoc-pkgs (scope &optional options)
  (DEBUG! "eldoc-pkgs scope %s options %s"
	  scope options)
  (let ((mode (plist-get options :mode)))
    (if mode
	(plist-get eldoc-pkgs-config
		   (mk-keyword (symbol-name mode)))
      nil)))

(defun eldoc-c/c++ (status)
  (DEBUG! "eldoc-c/c++ status %s"
	  status)
  (require 'c-eldoc)
  (if (> status 0)
      (c-turn-on-eldoc-mode)
    (c-turn-off-eldoc-mode)))
  
(defun eldoc (global status)
  (DEBUG! "eldoc global %s status %s major-mode %s"
	  global status major-mode)
  (if global
      (global-eldoc-mode status)
    (progn
      (eldoc-mode status)
      (pcase (symbol-name major-mode)
	("c-mode" (eldoc-c/c++ status))
	(_ t)))))

(defun activate-eldoc (scope &optional phase options)
  (DEBUG! "activate-eldoc scope %s phase %s options %s"
	  scope phase options)
  (if (>= (plist-get options :status) 0)
      (eldoc (eq scope 'editor) 1)
    (eldoc (eq scope 'editor) -1)))

(feature-ex! eldoc
	     "eldoc"
	     eldoc-pkgs
	     eldoc-config
	     eldoc-prepare
	     activate-eldoc)

