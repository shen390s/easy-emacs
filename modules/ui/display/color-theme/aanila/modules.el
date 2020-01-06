(package! :name aanila
	  :docstring "A dark emacs theme"
          :pkginfo (aanila :type git
			   :host github
			   :repo "santoshs/aanila"))

(defun aanila-activate ()
  (load-theme 'aanila t))

(feature! aanila
	  "A dark emacs theme"
          (aanila)
          nil
          aanila-activate
          nil)  
