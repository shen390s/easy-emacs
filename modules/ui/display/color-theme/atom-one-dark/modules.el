(package! :name atom-one-dark
	  :docstring "Atom One Dark theme for Emacs from Atom.io"
          :pkginfo (atom-one-dark :type git
				  :host github
				  :repo "emacsmirror/atom-one-dark-theme"))

(defun config-atom-one-dark ()
  t)

(defun enable-atom-one-dark ()
  (load-theme 'atom-one-dark t))

(feature! atom-one-dark
	  "Atom One Dark theme for Emacs"
          (atom-one-dark)
          config-atom-one-dark
          enable-atom-one-dark
          nil)
