(package-ex! projectile
	     "Projectile is a project interaction library for Emacs. 
Its goal is to provide a nice set of features operating on a project 
level without introducing external dependencies (when feasible)."
	     (projectile :type git
			 :host github
			 :repo "bbatsov/projectile"))

(defun activate-projectile (scope &optional phase options)
  (require 'projectile)

  (pcase scope
    ('app (progn
	    (let ((status (plist-get options :status)))
	      (when (and status
			 (>= status 0))
		(projectile-mode 1)))))
    (_ t)))

(feature-ex! projectile
	     "Projectile is a project interaction library for Emacs. 
Its goal is to provide a nice set of features operating on a project 
level without introducing external dependencies (when feasible)."
	     (projectile)
	     nil
	     nil
	     activate-projectile)
