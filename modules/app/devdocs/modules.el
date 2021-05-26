(package! devdocs-lookup
	     "Search the word at point with devdocs"
	     (devdocs-lookup :type git
			     :host github
			     :repo "xuchunyang/DevDocs.el"))

(defun activate-devdocs (scope &optional phase options)
  (require 'devdocs)
  (when (> (plist-get options) 0)
    (global-set-key "\C-cds" #'devdocs-search)))

(feature! devdocs
	     "Search the word at point with Devdocs"
	     (devdocs-lookup)
	     nil
	     nil
	     activate-devdocs)
