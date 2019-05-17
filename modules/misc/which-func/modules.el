(defun config-which-func ()
  ;;(setq mode-line-format (delete (assoc 'which-func-mode
  ;;					mode-line-misc-info)
  ;;				 mode-line-misc-info)
  ;;	which-func-header-line-format '(which-func-mode ("" which-func-format)  ;;))
  t)

(defun enable-which-func ()
  (which-function-mode))

(feature! which-func
	  "Show the name of current function"
	  nil
	  config-which-func
	  enable-which-func
	  nil)
