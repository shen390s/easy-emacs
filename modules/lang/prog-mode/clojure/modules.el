(scope! clojure prog-lang )

(package! clojure-mode
	  "A major Emacs mode for edit Clojure source code"
	  (clojure-mode :type git
	                :host github
			:repo "clojure-emacs/clojure-mode"))

(defun config-clojure ()
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.clj\\'" . clojure-mode))
    (attach! clojure clojure-mode)
    t))

(feature! clojure
	  "Support to edit clojure source code"
	  (smartparen-module clojure-mode)
	  config-clojure
	  nil
	  nil)


