
(package! :name rust-mode
	  :docstring "A major Emacs mode for edit Rust source code"
	  :pkginfo (rust-mode :type git
			      :host github
			      :repo "rust-lang/rust-mode"))

(autoload-r! rust-mode
	     (rust-mode)
	     "rust-mode"
	     t)


