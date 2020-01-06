(scope! clojure prog-lang )

(package! :name clojure-mode
	  :docstring "A major Emacs mode for edit Clojure source code"
	  :pkginfo (clojure-mode :type git
				 :host github
				 :repo "clojure-emacs/clojure-mode"))

(defun config-clojure ()
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.clj\\'" . lang/clojure-mode))
    t))

(autoload-r! clojure-mode
	     (clojure-mode)
	     "clojure-mode"
	     t)

(rmode! lang/clojure-mode
	"Emacs mode for clojure"
	(clojure-mode)
	config-clojure
	clojure-mode)

