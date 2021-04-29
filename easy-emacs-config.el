;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded

(easy! :vars
       (a . 10)
       (b . 20)
       :modes
       (c :suffix .c .cc .cpp .c++ .h .hpp
	  :features +lsp -flymake)
       (lisp :suffix .cl .el .lisp
	     :features +hlinum)
       :ui
       (theme my-theme)
       (font font1)
       :completion ivy -helm -autoc-complete
       :app (emacs-server :port 1234))
