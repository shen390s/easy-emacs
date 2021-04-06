
(package! :name yaml-mode
	  :docstring "A major Emacs mode for edit yaml document"
	  :pkginfo (yaml-mode :type git
			      :host github
			      :repo "yaml/yaml-mode"))

(defun config-yaml ()
  (DEBUG! "configuring yam mode...")
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.yml\\'" . lang/yaml-mode))
    t))

(autoload-r! yaml-mode
	     (yaml-mode)
	     "yaml-mode"
	     t)

(rmode! lang/yaml-mode
	"Emacs mode for yaml"
	(yaml-mode)
	config-yaml
	yaml-mode)

