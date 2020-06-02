(defvar QUILT (executable-find "quilt")
  "The executable file of quilt")

(defun emacs-quilt-check ()
  QUILT)

(defun emacs-quilt ()
  (when QUILT
    (let ((file (buffer-file-name)))
      (let ((dir (file-name-directory file)))
	(when (shell-command (format "cd %s && quilt top"
				     dir)
			     "QUILT Output")
	  (shell-command (format "cd %s && quilt add %s"
				 dir file)
			 "QUILT Output"))))))

(defun activate-emacs-quilt ()
  (add-hook 'before-save-hook #'emacs-quilt))

(defun deactivate-emacs-quilt ()
  (remove-hook 'before-save-hook #'emacs-quilt))

(feature!
 emacs-quilt
 "Emacs Editor Quilt"
 nil
 emacs-quilt-check
 activate-emacs-quilt
 deactivate-emacs-quilt)

