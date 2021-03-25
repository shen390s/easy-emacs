(defun lang-server-client ()
  (cond
   ((feature-in-scope 'eglot current-scope) 'eglot)
   (t 'lsp)))

