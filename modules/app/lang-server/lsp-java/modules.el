(package! lsp-java
	     "LSP support for Java"
	     (lsp-java :type git
		       :host github
		       :repo "emacs-lsp/lsp-java"))

(defun activate-java-lsp (scope &optional phase options)
  (require 'lsp-java)
  (let ((status (plist-get options :status)))
    (if (and status
	     (>= status 0))
	(lsp)
      (lsp-mode -1))))

(feature! lsp-java
	     "LSP for java"
	     (lsp-module lsp-ui lsp-java)
	     nil
	     nil
	     activate-java-lsp)
