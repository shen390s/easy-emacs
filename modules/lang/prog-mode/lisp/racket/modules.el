
(package! :name racket-mode
	  :docstring "GNU Emacs major modes for Racket: Edit and REPL. "
	  :pkginfo (racket-mode :type git
				:host github
				:repo "greghendershott/racket-mode"))

(package! :name pollen-mode
	  :docstring "An Emacs major mode for editing in pollen markup language "
	  :pkginfo (pollen-mode :type git
				:host github
				:repo "lijunsong/pollen-mode"))

(autoload-r! racket-mode
	     (racket-mode)
	     "racket-mode"
	     t)

(autoload-r! pollen-mode
	     (pollen-mode)
	     "pollen-mode"
	     t)

