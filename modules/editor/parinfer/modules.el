(package! :name parinfer
	  :docstring "Parinfer is a proof-of-concept editor mode for Lisp programming languages. It will infer some changes to keep Parens and Indentation inline with one another. "
	  :pkginfo (parinfer :type git
			     :host github
			     :repo "DogLooksGood/parinfer-mode"))

(defun config-parinfer ()
  (progn
    (setq parinfer-extensions (list 'defaults
				    'pretty-parens
				    'lispy
				    'paredit
				    'smart-yank))
    (when (feature-enabled 'evil)
      (setq parinfer-extensions (append parinfer-extensions '(evil))))
    t))

(defun enable-parinfer ()
  (when (feature-enabled 'evil)
    (require 'evil))
  
  (require 'lispy)
  (require 'paredit)
  (require 'parinfer)
  (require 'parinfer-ext)
  (parinfer-mode 1))

(defun deactive-parinfer ()
  (require 'parinfer)
  (parinfer-mode 0))

(feature! parinfer
	  "Parinfer"
	  (parinfer lispy paredit)
	  config-parinfer
	  enable-parinfer
	  deactive-parinfer)
