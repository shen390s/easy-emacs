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
    (let ((keys (sort-scopes (filt-out-non-keywords config))))
      (DEBUG! "scope keywords = %s" keys)
      (let ((c (collect-lists nil
			      (cl-loop for key in keys
				       collect `,@(make-scope-by-config key
									(plist-get config
										   key))))))
	(DEBUG! "c = %s" c)
	c))))

(defun easy-init ()
  t)

(defmacro easy! (&rest args)
  (let ((n-args (normalize-options args)))
    (DEBUG! "easy! n-args = \n%s"
	    (pp-to-string n-args))
    (let ((scopes (mk-scopes n-args)))
      ;; do something initial works
      (cl-loop for scope in scopes
	       do (Scope/init scope))
      (DEBUG! "scopes = \n%s"
	      (cl-loop for scope in scopes
		       collect (Scope/to-string scope)))
      (let ((c1 (cl-loop for action in '(prepare configure)
			 append (cl-loop for phase in '(before primary after)
					 append (cl-loop for scope in scopes
							 append (Scope/Code:get scope action phase)))))
	    (c2 `((setf actived-packages
			(append ,@(cl-loop for scope in scopes
					   append (Scope/Code:get scope 'pkglist 'ignore))))))
	    (c3 (cl-loop for scope in scopes
			 append (Scope/Code:get scope 'activate 'ignore))))
	(let ((code (append c1 c2 c3)))
	  (DEBUG! "easy! code = \n%s"
		  (pp-to-string code))
	  `(progn
	     (easy-emacs-bootstrap-core)
	     ,@code
	     (easy-emacs-boot-done)))))))
	
(provide 'core-config)
