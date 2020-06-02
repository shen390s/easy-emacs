(defun emacs-quilt-check ()
  t)

(defun emacs-quilt ()
  (INFO! "emacs-quilt")
  t)

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

