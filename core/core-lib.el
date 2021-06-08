;; -*- lexical-binding: t -*-
;; enable lexical scope

;;(require 'subr)

(defun collect-lists (acc lists)
  (if (null lists)
      acc
    (collect-lists (append acc (car lists))
		   (cdr lists))))

(defun first (lists)
  (nth 0 lists))

(defun second (lists)
  (nth 1 lists))

(defun third (lists)
  (nth 2 lists))

(defmacro safe-pop (lst)
  `(if (null ,lst)
       nil
     (pop ,lst)))
      
(defun file-is-under-directory (file dir)
  (let ((full-name (expand-file-name file))
	(full-dir (expand-file-name dir)))
    (progn
      (declare-function in-dir "core-lib")
      (defun in-dir (f d)
	(let ((f (string-trim-right f "/"))
	      (d (string-trim-right d "/")))
	  (cond ((string= f "") nil)
		((string= f d) t)
		(t (in-dir (file-name-directory f) d)))))
      (if (fboundp 'in-dir)
	  (in-dir full-name full-dir)
	nil))))

(defun file-in-path-list (file l)
  (cond ((null l) nil)
	((file-is-under-directory file (car l)) t)
	(t (file-in-path-list file (cdr l)))))

(defmacro when-bind! (var exp &rest body)
  (declare (indent defun))
  `(let ((,var ,exp))
     (when ,var
       ,@body)))

(defmacro set-vars (&rest vals)
  `(progn
     ,@(let ((vars vals))
	 (cl-loop until (null vars)
		  collect (let* ((var (safe-pop vars))
				 (val (safe-pop vars)))
			    `(setq ,var ,val))))))

(defmacro when-call! (fun-or-macro &rest args)
  `(unless (equal ',fun-or-macro 'nil)
     (,fun-or-macro ,@args)))

(defmacro apply-macro (mac &rest args)
  `(,mac ,@args))
  
(let ((zval 0))
  (defun gen_seq ()
    (let ((rval zval))
      (setq zval (1+ zval))
      rval)))

(defun remove-all-assoc (key alist)
  (let ((e (assoc key alist)))
    (if e
	(remove-all-assoc key (delq e alist))
      alist)))

(defun reassoc-ext (mode1 mode2)
  (setq auto-mode-alist
	(cl-loop for it in auto-mode-alist
		 collect (let ((mode (cdr it)))
			   (if (eq mode mode1)
			       (cons (car it) mode2)
			     it)))))
(eval-and-compile
  (defun dummy-fn ()
    t))

(eval-when-compile
  (defun mk-action (action)
    (if action
	`,@action
      `((dummy-fn)))))

(defmacro trace! (fn &optional action)
  `(advice-add ',fn
	       :around
	       #'(lambda (origin-fun &rest args)
		   ,@(mk-action action)
		   (DEBUG! "call %s args: %s"
			   origin-fun args)
		   (let ((result (apply origin-fun args)))
		     (progn
		       (DEBUG! "result: %s" result)
		       result)))))

(defun git-branch (dir)
  (replace-regexp-in-string
   "[ \t]+" ":"
   (replace-regexp-in-string
    "[\n\r]+" ""
    (shell-command-to-string
     (format "cd %s && git branch --show-current"
	     dir)) "\n")))

(defun keyword-name (keyword)
  (if (keywordp keyword)
      (substring (symbol-name keyword) 1)
    nil))

(defun filt-out-non-keywords (l)
  (cl-remove-if-not (lambda (k)
		      (keywordp k))
		    l))

(defun filt-key-args (collected-args keys args)
  (if (null args)
      (reverse collected-args)
    (let ((x (car args))
	  (remain (cdr args)))
      (cond
       ((member x keys)
	(if (null remain)
	    (reverse collected-args)
	  (filt-key-args collected-args keys (cdr remain))))
       (t (filt-key-args (push x collected-args) keys remain))))))

(defun collect-keyword-values (kv-list)
  (let ((result nil)
	(key    :default)
	(values nil))
    (cl-loop for item in kv-list
	     do (if (keywordp item)
		    (progn
		      (setq result
			    (plist-put result key (nreverse values)))
		      (setq key item)
		      (setq values nil))
		  (push item values)))
    (when values
      (setq result
	    (plist-put result key (nreverse values))))
    result))

(defun plist-append-key-values (plist key value)
  (plist-put plist key
	     (append (plist-get plist key)
		     (list value))))

(defun assoc-suffix-to-mode (suffix mode)
  (let ((s (format "\\%s\\'" suffix))
	(m (intern (format "%s-mode" mode))))
    (DEBUG! "assoc suffix %s to mode %s"
	    suffix mode)
    (add-to-list 'auto-mode-alist
		 `(,s . ,m))))

(defun gen-add-suffix-to-mode (suffix mode)
  (let ((s (format "\\%s\\'" suffix))
	(m (intern (format "%s-mode" mode))))
    `(add-to-list 'auto-mode-alist
		  '(,s . ,m))))

(defun mk-keyword (name)
  (intern (format ":%s" name)))

(defun normalize-non-keyword-options (options)
  (collect-lists nil
		 (cl-loop for option in options
			  collect (let ((v (symbol-name option)))
				    (pcase (substring v 0 1)
				      ("-" `(,(mk-keyword
					       (substring v 1)) -1))
				      ("+" `(,(mk-keyword
					       (substring v 1)) 1))
				      (_ `(,(mk-keyword v) 0)))))))

(defun normalize-options (options)
  (let ((c-key :default)
	(z-options nil))
    (cl-loop for item in options
	     do (if (keywordp item)
		    (setq c-key item)
		  (setq z-options
			(plist-append-key-values z-options
						 c-key
						 item))))
    (setq z-options
	  (plist-put z-options
		     :default
		     (normalize-non-keyword-options (plist-get z-options
							       :default))))
    z-options))

(defun indirect-buffer-p (&optional buffer)
  (buffer-base-buffer buffer))

(defun prepend-env (s env-var)
  (let ((env-val (getenv env-var)))
    (setenv env-var
	    (if env-val
		(format "%s:%s" s env-val)
	      s))))

(defun emacs-idle-p ()
  (let ((idle-time (current-idle-time)))
    (and idle-time
	 (let ((idle-h (nth 0 idle-time))
	       (idle-l (nth 1 idle-time)))
	   (or (> idle-h 0)
	       (>= idle-l 1))))))

(defmacro with-file! (filename &rest body)
  `(append-to-file (progn
		     ,@body)
		   nil
		   ,filename))

(provide 'core-lib)
