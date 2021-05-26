(package! dash-at-point
	     "Search the word at point with Dash"
	     (dash-at-point :type git
			    :host github
			    :repo "stanaka/dash-at-point"))

(defun enable-dash (scope &optional phase options)
  (require 'dash-at-point)
  (when (> (plist-get options :status) 0)
    (progn
      (global-set-key "\C-cds" #'dash-at-point))))

(feature! dash
	     "Search the word at point with Dash"
	     (dash-at-point)
	     nil
	     nil
	     activate-dash)
    
    
    
