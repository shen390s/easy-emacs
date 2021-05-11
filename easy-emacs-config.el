;; -*- lexical-binding: t -*-
;; Configuration for easy-emacs
;; this file will be loaded

(easy! :vars
       (a . 10)
       (b . 20)
       (c-eldoc-includes . "-I/usr/include -I/usr/local/include -I. -I..")
       :modes
       (prog :features +hlinum +ruler +rainbow-delimiters
	     +rainbow-identifiers +smartparens -flymake)
       (c :suffix .c .cc .cpp .c++ .h .hpp
	  :features +eldoc)
       (emacs-lisp :suffix .el)
       (lisp :suffix .cl .lisp)
       :ui
       (evil
	:after-activate (progn
			  (DEBUG! "evil set leader key")
			  (evil-leader/set-leader "<SPC>")
			  (evil-leader/set-key
			    (kbd "bs") 'counsel-switch-buffer
			    (kbd "f") 'counsel-find-file
			    (kbd "wm") 'delete-other-windows
			    (kbd "bd") 'kill-buffer
			    (kbd "tc") 'transpose-chars
			    (kbd "tw") 'transpose-words
			    (kbd "tc") 'transpose-regions
			    (kbd "tp") 'transpose-paragraphs
			    (kbd "tse") 'transpose-sexps
			    (kbd "tst") 'transpose-sentences)
			  ;; for the problem of evil-leader-mode
			  ;; has not been enabled for some special
			  ;; buffer
			  (cl-loop for b in (buffer-list)
				   do (with-current-buffer b
					(evil-leader-mode 1)))))
       (smart-mode-line)
       (load-custom :theme rshen)
       (smex)
       :completion ivy 
       :editor
       (undo-tree)
       ;;(-eldoc)
       :app
       (emacs-server)
       (which-key)
       (lang-server +cquery -ccls -clangd +eglot))
