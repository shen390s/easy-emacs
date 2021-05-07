;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded

(easy! :vars
       (a . 10)
       (b . 20)
       :modes
       (prog :features +hlinum +ruler +rainbow-delimiters
	     +rainbow-identifiers)
       (c :suffix .c .cc .cpp .c++ .h .hpp
	  :features +lsp +eldoc -flymake)
       (lisp :suffix .cl .el .lisp)
       :ui
       (evil :after-activate
	     (progn
	       (DEBUG! "evil set leader key")
	       (evil-leader/set-leader "<SPC>")
	       (evil-leader/set-key
		 (kbd "b") 'counsel-switch-buffer
		 (kbd "f") 'counsel-find-file)))
       (smart-mode-line)
       (load-custom :theme rshen)
       :completion ivy 
       :app (emacs-server :port 1234))
