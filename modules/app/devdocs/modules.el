(package! :name devdocs-lookup
	  :docstring "Search the word at point with devdocs"
	  :pkginfo (devdocs-lookup :type git
				   :host github
				   :repo "xuchunyang/DevDocs.el"))

(defun config-devdocs ()
  t)

(defun enable-devdocs ()
    (progn
      (require 'devdocs)
      (global-set-key "\C-cds" #'devdocs-search)))

(feature! devdocs
	  "Search the word at point with Devdocs"
	  (devdocs-lookup)
	  config-devdocs
	  enable-devdocs
	  nil)
