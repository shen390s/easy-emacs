(defvar QUILT (executable-find "quilt")
  "The executable file of quilt")

(defun emacs-quilt-check (scope &optional phase options)
  (DEBUG! "emacs-quilt-check scope %s phase %s options %s"
	  scope phase options)
  QUILT)

(defun emacs-quilt ()
  (when QUILT
    (let ((file (buffer-file-name)))
      (let ((dir (file-name-directory file)))
	(when (shell-command (format "cd %s && quilt top"
				     dir)
			     "QUILT Output")
	  (shell-command (format "cd %s && quilt add %s"
				 dir (file-name-nondirectory file))
			 "QUILT Output"))))))

(defun emacs-quilt/:activate ()
  (add-hook 'before-save-hook #'emacs-quilt))

(defun emacs-quilt/:deactivate ()
  (remove-hook 'before-save-hook #'emacs-quilt))

(defun activate-emacs-quilt (scope &optional phase options)
  (let ((status (plist-get options :status)))
    (when status
      (if (>= status 0)
	  (emacs-quilt/:activate)
	(emacs-quilt/:deactivate)))))

(feature! emacs-quilt
	     "Emacs Editor Quilt"
	     nil
	     emacs-quilt-check
	     nil
	     activate-emacs-quilt)

