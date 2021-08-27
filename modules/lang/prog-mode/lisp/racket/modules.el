;; -*- lexical-binding: t -*-

(package! racket-mode
	     "GNU Emacs major modes for Racket: Edit and REPL. "
	     (racket-mode :type git
			  :host github
			  :repo "greghendershott/racket-mode"))

(package! pollen-mode
	     "An Emacs major mode for editing in pollen markup language "
	     (pollen-mode :type git
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

