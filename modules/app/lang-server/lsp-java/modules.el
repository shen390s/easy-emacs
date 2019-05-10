(package!
 lsp-java
 "LSP support for Java"
 (lsp-java :type git :host github :repo "emacs-lsp/lsp-java"))

(defun enable-java-lsp ()
  (require 'lsp-java)
  (enable-lsp))

(feature!
 lsp-java
 "LSP for java"
 (lsp-module lsp-ui lsp-java)
 nil
 enable-java-lsp
 nil)
