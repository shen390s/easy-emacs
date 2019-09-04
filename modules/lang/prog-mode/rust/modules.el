(scope! rust prog-lang)

(package! rust-mode
	  "A major Emacs mode for edit Rust source code"
	  (rust-mode :type git
		     :host github
		     :repo "rust-lang/rust-mode"))

(defun config-rust ()
  (add-to-list 'auto-mode-alist
	       '("\\.rs\\'" . lang/rust-mode))
  t)

(autoload-r! rust-mode
	     (rust-mode)
	     "rust-mode"
	     t)

(mode! lang/rust-mode
       "Emacs mode for rust program language"
       (rust-mode)
       config-rust
       rust-mode)

