(scope! racket prog-lang)

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

(defun config-racket ()
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.rkt\\'" . lang/racket-mode))
    (global-set-key "\M-\\" "◊")
    t))

(autoload-r! racket-mode
	     (racket-mode)
	     "racket-mode"
	     t)

(rmode! lang/racket-mode
	"Emacs mode for racket edit and REPL"
	(racket-mode)
	config-racket
	racket-mode)

(autoload-r! pollen-mode
	     (pollen-mode)
	     "pollen-mode"
	     t)

(defun config-pollen ()
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.poly\\.pm\\'" . lang/pollen-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.scrbl\\'" . lang/pollen-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.pmd\\'" . lang/pollen-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.pp\\'" . lang/pollen-mode))
    t))

(rmode! lang/pollen-mode
	"Emacs mode for editing pollen markup language"
	(pollen-mode)
	config-pollen
	pollen-mode)
