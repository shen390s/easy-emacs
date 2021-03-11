(package! :name dash-at-point
	  :docstring "Search the word at point with Dash"
	  :pkginfo (dash-at-point :type git
				  :host github
				  :repo "stanaka/dash-at-point"))

(defun config-dash ()
  t)

(defun enable-dash ()
  (require 'dash-at-point)
    (progn
      (global-set-key "\C-cds" #'dash-at-point)))

(feature! dash
	  "Search the word at point with Dash"
	  (dash-at-point)
	  config-dash
	  enable-dash
	  nil)
    
    
    
