(package-ex!  treemacs
	      "Treemacs can be extended to display arbitrary nodes 
as well as be used as a general rendering backend for any tree-like 
structures."
	      treemacs)

(package-ex!  treemacs-evil
	      "treemacs evil integration"
	      treemacs-evil)

(package-ex!  treemacs-magit
	      "treemacs magit integration"
	      treemacs-magit)

(package-ex!  treemacs-projectile
	      "treemacs projectile integration"
	      treemacs-projectile)

(package-ex!  treemacs-icons-dired
	      "treemacs with icons dired"
	      treemacs-icons-dired)

(defun treemacs-pkgs (scope &optional phase options)
  (let ((pkgs '(treemacs treemacs-icons-dired)))
    (when (feature-on :evil options)
      (push 'treemacs-evil pkgs))
    (when (feature-on :magit options)
      (push 'treemacs-magit pkgs))
    (when (feature-on :projectile options)
      (push 'treemacs-projectile pkgs))
    pkgs))

(defun activate-treemacs (scope &optional phase options)
  (require 'dired)

  (pcase scope
    ('app (progn
	    (let ((status (plist-get options :status)))
	      (when (and status
			 (>= status 0))
		(treemacs)
		(treemacs-icons-dired-mode)))))
    (_ t)))

(feature-ex! treemacs
	     "Treemacs can be extended to display arbitrary nodes 
as well as be used as a general rendering backend for any tree-like 
structures."
	     treemacs-pkgs
	     nil
	     nil
	     activate-treemacs)
