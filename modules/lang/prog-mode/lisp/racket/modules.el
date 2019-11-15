(scope! racket prog-lang)

(package! racket-mode
	  "GNU Emacs major modes for Racket: Edit and REPL. "
	  (racket-mode :type git
		       :host github
		       :repo "greghendershott/racket-mode"))

(defun config-racket ()
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.rkt\\'" . lang/racket-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.poly\\.pm\\'" . lang/racket-mode))
    (add-to-list 'auto-mode-alist
		 '("\\.scrbl\\'" . lang/racket-mode))
    (global-set-key "\M-\\" "◊")
    t))

(autoload-r! racket-mode
	     (racket-mode)
	     "racket-mode"
	     t)

(mode! lang/racket-mode
       "Emacs mode for racket edit and REPL"
       (racket-mode)
       config-racket
       racket-mode)
