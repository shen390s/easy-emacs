(package! :name devdocs-lookup
	  :docstring "Search the word at point with devdocs"
	  :pkginfo (devdocs-lookup :type git
				   :host github
				   :repo "kaushalmodi/devdocs-lookup"))

(defun config-devdocs ()
  t)

(defun enable-devdocs ()
    (progn
      (require 'devdocs-lookup)
      (devdocs-setup)
      (global-set-key "\C-cds" #'devdocs-lookup)))

(feature! devdocs
	  "Search the word at point with Devdocs"
	  (devdocs-lookup)
	  config-devdocs
	  enable-devdocs
	  nil)
