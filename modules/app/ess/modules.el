(package! :name ess
	  :docstring "Emacs Speaks Statistics"
          :pkginfo ess)

(defun config-ess ()
  (executable-find "R"))

(defun activate-ess ()
  (load "ess-autoloads"))
  
(feature! ess
	  "Emacs Speaks Statistics"
          (ess)
          config-ess
          activate-ess
          nil)          
