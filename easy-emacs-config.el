;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded

(easy! :vars
       (a . 10)
       (b . 20)
       :modes
       (prog :features +hlinum)
       (c :suffix .c .cc .cpp .c++ .h .hpp
	  :features +lsp -flymake -hlinum)
       (lisp :suffix .cl .el .lisp)
       :ui
       (theme my-theme)
       (font font1)
       :completion ivy 
       :app (emacs-server :port 1234))
