(package! :name tsc
	  :docstring "tree-sitter core"
	  :pkginfo (tsc :type git
			:host github
			:repo "ubolonton/emacs-tree-sitter"
			:files ("core/*.el")))

(package! :name tree-sitter
	  :docstring "tree-sitter framework"
	  :pkginfo (tree-sitter :type git
				:host github
				:repo "ubolonton/emacs-tree-sitter"
				:files ("lisp/*.el")))

(package! :name tree-sitter-langs
	  :docstring "tree-sitter languages bundle"
	  :pkginfo (tree-sitter-langs :type git
				      :host github
				      :repo
				      "ubolonton/emacs-tree-sitter"
				      :files ("langs/*.el"
					      "langs/queries")))

(defun config-tree-sitter ()
  t)

(defun activate-tree-sitter ()
  (require 'tree-sitter-langs)
  (tree-sitter-mode)
  (tree-sitter-hl-mode))

(defun deactivate-tree-sitter ()
  (tree-sitter-hl-mode -1)
  (tree-sitter-mode -1))

(feature! tree-sitter
	  "enable tree-sitter for buffer local syntax highlight"
	  (tsc tree-sitter tree-sitter-langs)
	  config-tree-sitter
	  activate-tree-sitter
	  deactivate-tree-sitter)

