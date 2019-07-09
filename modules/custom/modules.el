(defun config-load-custom ()
  t)

(defun enable-load-custom ()
  (custom-set-variables
   '(lsp-prefer-flymake nil))
  (load custom-file t t))

(feature! load-custom
	  "load customization file"
	  nil
	  config-load-custom
	  enable-load-custom
	  nil)
