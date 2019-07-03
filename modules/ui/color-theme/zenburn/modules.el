(package! zenburn-emacs
	  "The Zenburn colour theme ported to Emacs"
	  (zenburn-emacs :type git
			 :host github
			 :repo "bbatsov/zenburn-emacs"))

(defun config-zenburn-emacs ()
  t)

(defun enable-zenburn-emacs ()
  (load-theme 'zenburn t))

(feature! zenburn-emacs
	  "The Zenburn colour theme ported to Emacs"
	  (zenburn-emacs)
	  config-zenburn-emacs
	  enable-zenburn-emacs
	  nil)
