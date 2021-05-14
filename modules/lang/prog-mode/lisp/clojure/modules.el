
(package-ex! clojure-mode
	     "A major Emacs mode for edit Clojure source code"
	     (clojure-mode :type git
			   :host github
			   :repo "clojure-emacs/clojure-mode"))

(autoload-r! clojure-mode
	     (clojure-mode)
	     "clojure-mode"
	     t)


