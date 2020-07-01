(scope! ascii-doctor prog-lang)

(package! :name adoc-mode
	  :docstring "A major Emacs mode for edit ascii-doctor document"
	  :pkginfo (fish-mode :type git
			      :host github
			      :repo "sensorflo/adoc-mode"))

(defun config-adoc ()
  (add-to-list 'auto-mode-alist
	       '("\\.adoc\\'" . lang/adoc-mode))
  t)

(autoload-r! adoc-mode
	     (adoc-mode)
	     "ascii-doctor mode"
	     t)

(rmode! lang/adoc-mode
	"Emacs mode for ascii-doctor"
	(adoc-mode)
	config-adoc
	adoc-mode)

