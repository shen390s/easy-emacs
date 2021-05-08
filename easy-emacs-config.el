;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded

(easy! :vars
       (a . 10)
       (b . 20)
       :modes
       (prog :features +hlinum +ruler +rainbow-delimiters
	     +rainbow-identifiers +smartparens)
       (c :suffix .c .cc .cpp .c++ .h .hpp
	  :features +lsp +eldoc -flymake)
       (emacs-lisp :suffix .el)
       (lisp :suffix .cl .lisp)
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
       :editor
       (undo-tree)
       (-eldoc)
       :app (emacs-server :port 1234))
