(package! evil-surround
	     "This package emulates surround.vim by Tim Pope. 
The functionality is wrapped into a minor mode.
This package uses Evil as its vi layer."
	     (evil-surround :type git
			    :host github
			    :repo "emacs-evil/evil-surround"))

;; use autoload-r! to ensure
;; evil-surround to be installed
(autoload-r! global-evil-surround-mode
	     (evil-surround)
	     "evil-surround"
	     t)

(defun config-evil-surround (scope &optional phase options)
  (pcase scope
    ('editor (let ((status (plist-get options :status)))
               (when (and status
			  (>= status 0))
		 (progn
		   (after-activate! evil
				    (progn
				      (global-evil-surround-mode 1)))))))
    (_ t)))
  
(feature! evil-surround
	     "This package emulates surround.vim by Tim Pope. 
The functionality is wrapped into a minor mode.
This package uses Evil as its vi layer."
	     (evil-surround)
	     config-evil-surround
	     nil
	     nil)
       
               
