
(package! :name go-mode
	  :docstring "A major Emacs mode for edit Go source code"
	  :pkginfo (go-mode :type git
			    :host github
			    :repo "dominikh/go-mode.el"))

(defun config-go ()
  (add-to-list 'auto-mode-alist
	       '("\\.go\\'" . lang/go-mode))
  t)

(autoload-r! go-mode
	     (go-mode)
	     "go-mode"
	     t)

(rmode! lang/go-mode
	"Emacs mode for go program language"
	(go-mode)
	config-go
	go-mode)

