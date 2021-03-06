(package! :name forge
	  :docstring "Work with git forge from comfort of magit"
          :pkginfo (forge :type git
			  :host github
			  :repo "magit/forge"))

(defun config-forge ()
  t)

(defun activate-forge ()
  (require 'magit)
  (require 'forge))

(feature! forge
	  "work with git forge from comfort of magit"
          (magit forge)
          config-forge
          activate-forge
          nil)
