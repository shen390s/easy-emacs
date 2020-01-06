(package! :name treemacs
	  :docstring "Treemacs can be extended to display arbitrary nodes as well as be used as a general rendering backend for any tree-like structures."
	  :pkginfo treemacs)

(package! :name treemacs-evil
	  :docstring "treemacs evil integration"
	  :pkginfo treemacs-evil)

(package! :name treemacs-magit
	  :docstring "treemacs magit integration"
	  :pkginfo treemacs-magit)

(package! :name treemacs-projectile
	  :docstring "treemacs projectile integration"
	  :pkginfo treemacs-projectile)

(package! :name treemacs-icons-dired
	  :docstring "treemacs with icons dired"
	  :pkginfo treemacs-icons-dired)

(defun treemacs-pkgs ()
  (let ((pkgs '(treemacs treemacs-icons-dired)))
    (when (feature-enabled 'evil)
      (push 'treemacs-evil pkgs))
    (when (feature-enabled 'magit)
      (push 'treemacs-magit pkgs))
    (when (feature-enabled 'projectile)
      (push 'treemacs-projectile pkgs))
    pkgs))

(defun enable-treemacs ()
  (treemacs)
  (require 'dired)
  (treemacs-icons-dired-mode))

(feature!
 treemacs
 "Treemacs can be extended to display arbitrary nodes as well as be used as a general rendering backend for any tree-like structures."
 treemacs-pkgs
 nil
 enable-treemacs
 nil)
