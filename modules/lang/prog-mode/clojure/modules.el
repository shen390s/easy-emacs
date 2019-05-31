(scope! clojure prog-lang clojure-mode)

(package! clojure-mode
	  "A major Emacs mode for edit Clojure source code"
	  (clojure-mode :type git
	                :host github
			:repo "clojure-emacs/clojure-mode"))

(defun config-clojure ()
  (add-to-list 'auto-mode-alist
	       '("\\.clj\\'" . clojure-mode)))

(feature! clojure
	  "Support to edit clojure source code"
	  (smartparen-module clojure-mode)
	  config-clojure
	  nil
	  nil)


