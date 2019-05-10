(package!
 treemacs
 "Treemacs can be extended to display arbitrary nodes as well as be used as a general rendering backend for any tree-like structures."
 treemacs)

(package!
 treemacs-evil
 "treemacs evil integration"
 treemacs-evil)

(package!
 treemacs-magit
 "treemacs magit integration"
 treemacs-magit)

(package!
 treemacs-projectile
 "treemacs projectile integration"
 treemacs-projectile)

(defun treemacs-pkgs ()
  (let ((pkgs '(treemacs)))
    (when (feature-enabled 'evil)
      (push 'treemacs-evil pkgs))
    (when (feature-enabled 'magit)
      (push 'treemacs-magit pkgs))
    (when (feature-enabled 'projectile)
      (push 'treemacs-projectile pkgs))
    pkgs))

(defun enable-treemacs ()
  (treemacs))

(feature!
 treemacs
 "Treemacs can be extended to display arbitrary nodes as well as be used as a general rendering backend for any tree-like structures."
 treemacs-pkgs
 nil
 enable-treemacs
 nil)
