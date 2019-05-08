(scope! rust (rust-mode-hook) nil)

(package!
 rust-mode
 "A major Emacs mode for edit Rust source code"
 (rust-mode :type git :host github :repo "rust-lang/rust-mode"))

(feature!
 rust
 "Support to edit rust source code"
 (rust-mode)
 nil
 nil
 nil)


