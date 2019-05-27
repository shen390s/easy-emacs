(scope! rust rust-mode)

(package! rust-mode
	  "A major Emacs mode for edit Rust source code"
	  (rust-mode :type git :host github :repo "rust-lang/rust-mode"))

(defun config-rust ()
  (add-to-list 'auto-mode-alist
	       '("\\.rs\\'" . rust-mode))
  t)

(feature! rust
	  "Support to edit rust source code"
	  (rust-mode)
	  config-rust
	  nil
	  nil)


