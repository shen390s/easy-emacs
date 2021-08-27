;; -*- lexical-binding: t -*-
(package! sanityinc-tomorrow
	     "A set of comprehensive Emacs color themes based on 
Chris Kempson's 'tomorrow' themes"
	     (sanityinc-tomorrow :type git
				 :host github
				 :repo "purcell/color-theme-sanityinc-tomorrow"))

(defvar sanityinc-theme-actived nil
  "Activated sanityinc theme")

(defmacro load-sanityinc-tomorrow (actived)
  `(funcall (intern (concat "color-theme-sanityinc-tomorrow-"
			    (symbol-name ,actived)))))

(defun activate-sanityinc-tomorrow (scope &optional phase options)
  (require 'color-theme-sanityinc-tomorrow)
  (pcase scope
    ('ui (let ((status (plist-get options :status)))
	   (when (and status
		      (>= status 0))
	     (progn
	       (when anityinc-theme-actived
		 (load-sanityinc-tomorrow anityinc-theme-actived))))))
    (_ t)))

(feature! sanityinc-tomorrow
	  "A set of comprehensive Emacs color themes based on Chris Kempson's 'tomorrow' themes"
	  (sanityinc-tomorrow)
	  nil
	  nil
	  activate-sanityinc-tomorrow)
