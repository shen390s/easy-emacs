
(package! rust-mode
	     "A major Emacs mode for edit Rust source code"
	     (rust-mode :type git
			:host github
			:repo "rust-lang/rust-mode"))

(autoload-r! rust-mode
	     (rust-mode)
	     "rust-mode"
	     t)

(set-parent-mode 'rust-mode 'prog-mode)

