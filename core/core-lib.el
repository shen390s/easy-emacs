;; -*- lexical-binding: t -*-
;; enable lexical scope

;;(require 'subr)

(defun collect-lists (acc lists)
  (if (null lists)
      acc
    (collect-lists (append acc (car lists))
		   (cdr lists))))

(defun nth-car (n lists)
  (if (null lists)
      nil
    (if (= n 1)
	(car lists)
      (nth-car (- n 1)
	       (cdr lists)))))

(defun first (lists)
  (nth-car 1 lists))

(defun second (lists)
  (nth-car 2 lists))

(defun third (lists)
  (nth-car 3 lists))

(defmacro safe-pop (lst)
  `(if (null ,lst)
       nil
     (pop ,lst)))
      
(defun file-is-under-directory (file dir)
  (defun in-dir (f d)
    (let ((f (string-trim-right f "/"))
	  (d (string-trim-right d "/")))
      (cond ((string= f "") nil)
	    ((string= f d) t)
	    (t (in-dir (file-name-directory f) d)))))
  (let ((full-name (expand-file-name file))
	(full-dir (expand-file-name dir)))
    (in-dir full-name full-dir)))

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

(eval-and-compile
  (defun scope-null ()
    t)

  (defun scope-function (scope tag subtag)
    (if scope
	(intern (concat (symbol-name scope)
			"-scope-"
			(symbol-name tag)
			(symbol-name subtag)))
      (intern "scope-null"))))

(eval-and-compile
  (defun mode-function (mode)
    (intern (concat (symbol-name mode)
		    ":entry"))))

(defun remove-all-assoc (key alist)
  (let ((e (assoc key alist)))
    (if e
	(remove-all-assoc key (delq e alist))
      alist)))

(defun unassoc-ext (ext)
  (setq auto-mode-alist
	(remove-all-assoc ext auto-mode-alist)))

(eval-and-compile
  (defun dummy-fn ()
    t))

(defun mk-action (action)
  (if action
      `,@action
    `((dummy-fn))))

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

(provide 'core-lib)
