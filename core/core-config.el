;; -*- lexical-binding: t -*-

(require 'core-lib)
(require 'core-log)
(require 'core-modules)
(require 'core-features)
(require 'core-scope)

;; (easy! :vars
;;        (a . 10)
;;        (b . 20)
;;        :modes
;;        (prog :features +hlinum +ruler +rainbow-delimiters
;; 	     +rainbow-identifiers)
;;        (c :suffix .c .cc .cpp .c++ .h .hpp
;; 	  :features +lsp +eldoc -flymake)
;;        (lisp :suffix .cl .el .lisp)
;;        :ui
;;        (evil :after-activate
;; 	     (progn
;; 	       (DEBUG! "evil set leader key")
;; 	       (evil-leader/set-leader "<SPC>")
;; 	       (evil-leader/set-key
;; 		 (kbd "b") 'counsel-switch-buffer
;; 		 (kbd "f") 'counsel-find-file)))
;;        (smart-mode-line)
;;        (load-custom :theme rshen)
;;        :completion ivy 
;;        :app (emacs-server :port 1234))
(defvar  easy-emacs-config-data nil
  "Configuration data of easy-emacs")

(defun save-easy-config (config)
  (setf easy-emacs-config-data
	config))

(defun easy-config-get (scope)
  (plist-get easy-emacs-config-data scope))

(defun make-easy-config (configs)
  (let ((config (collect-keyword-values configs)))
    (DEBUG! "config = %s" (pp-to-string config))
    (save-easy-config config)
    (let ((keys (filt-out-non-keywords config)))
      (DEBUG! "scope keywords = %s" keys)
      (let ((c (collect-lists nil
			      (cl-loop for key in keys
				       collect `,@(make-scope-by-config key
									(plist-get config
										   key))))))
	(DEBUG! "c = %s" c)
	c))))

(defun easy-init ()
  (setq auto-mode-alist nil))

(defmacro easy! (&rest args)
  `(progn
     (easy-init)
     ,@(make-easy-config args)
     ,@(cl-loop for phase in '(before primary after)
		collect `(foreach-scope! scope-name scope
					 (,(intern (format
						    "Scope/Prepare:%s" phase)) scope)))
     (foreach-scope! scope-name scope
		     (defer-package-install (Scope/get-pkgs scope)))
     
     ,@(cl-loop for phase in '(before primary after)
		collect `(foreach-scope! scope-name
					 scope
					 (,(intern (format "Scope/Configure:%s" phase)) scope)))
     (foreach-scope! scope-name scope
		     (Scope/Activate scope))))


(provide 'core-config)
