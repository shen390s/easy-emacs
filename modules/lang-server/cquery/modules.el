(package!
 emacs-cquery
 "Use cquery as c/c++ language server"
 (emacs-cquery :type git :host github :repo "cquery-project/emacs-cquery"))

(defun cquery-on ()
  (require 'cquery)
  (lsp))

(feature!
 cquery
 "Use cquery as c/c++ language server"
 (lsp-module emacs-cquery)
 nil
 cquery-on
 nil)
