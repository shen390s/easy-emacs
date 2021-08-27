;; -*- lexical-binding: t -*-
(package!  tsc
	      "tree-sitter core"
	      (tsc :type git
		   :host github
		   :repo "ubolonton/emacs-tree-sitter"
		   :files ("core/*.el")))

(package!  tree-sitter
	      "tree-sitter framework"
	      (tree-sitter :type git
			   :host github
			   :repo "ubolonton/emacs-tree-sitter"
			   :files ("lisp/*.el")))

(package!  tree-sitter-langs
	      "tree-sitter languages bundle"
	      (tree-sitter-langs :type git
				 :host github
				 :repo
				 "ubolonton/emacs-tree-sitter"
				 :files ("langs/*.el"
					 "langs/queries")))

(defun activate-tree-sitter (scope &optional phase options)
  (require 'tree-sitter-langs)

  (pcase scope
    ('modes (progn
	      (let ((status (plist-get options :status)))
		(if (and status
			 (>= status 0))
		    (progn
		      (tree-sitter-mode 1)
		      (tree-sitter-hl-mode 1))
		  (progn
		    (tree-sitter-mode -1)
		    (tree-sitter-hl-mode -1))))))
    (_ t)))

(feature! tree-sitter
	     "enable tree-sitter for buffer local syntax highlight"
	     (tsc tree-sitter tree-sitter-langs)
	     nil
	     nil
	     activate-tree-sitter)

