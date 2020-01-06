(package! :name yasnippet
	  :docstring "YASnippet is a template system for Emacs"
	  :pkginfo (yasnippet :type git
			      :host github
			      :repo "joaotavora/yasnippet"))

(defun enable-yasnippet ()
  (yas-global-mode 1))

(feature! yasnippet
	  "YASnippet is a template system for Emacs"
	  (yasnippet)
	  nil
	  enable-yasnippet
	  nil)
