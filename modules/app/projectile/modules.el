(package!
 projectile
 "Projectile is a project interaction library for Emacs. Its goal is to provide a nice set of features operating on a project level without introducing external dependencies (when feasible)."
 (projectile :type git
	     :host github
	     :repo "bbatsov/projectile"))

(defun enable-projectile ()
  (projectile-mode 1))

(feature!
 projectile
 "Projectile is a project interaction library for Emacs. Its goal is to provide a nice set of features operating on a project level without introducing external dependencies (when feasible)."
 (projectile)
 nil
 enable-projectile
 nil)
