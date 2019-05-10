(package!
 treemacs
 "Treemacs can be extended to display arbitrary nodes as well as be used as a general rendering backend for any tree-like structures."
 treemacs)

(defun enable-treemacs ()
  (treemacs))

(feature!
 treemacs
 "Treemacs can be extended to display arbitrary nodes as well as be used as a general rendering backend for any tree-like structures."
 (treemacs)
 nil
 enable-treemacs
 nil)
