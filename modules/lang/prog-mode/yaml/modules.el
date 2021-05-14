
(package! :name yaml-mode
	  :docstring "A major Emacs mode for edit yaml document"
	  :pkginfo (yaml-mode :type git
			      :host github
			      :repo "yaml/yaml-mode"))

(autoload-r! yaml-mode
	     (yaml-mode)
	     "yaml-mode"
	     t)

