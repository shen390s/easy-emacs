(defun lang-server-client ()
  (cond
   ((member current-scope (feature-enabled 'eglot)) 'eglot)
   (t 'lsp)))

