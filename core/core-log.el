; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'pp)
(require 'core-lib)

(defvar log-level 0 "log level")  

(defvar log-levels nil
  "Pre-defined log levels")

(defvar log-time-fmt "%m/%d/%Y %T "
  "Default log format of time")

(defvar log-buffer-name "*EasyEmacs Logs*"
  "The buffer name of log buffer")

(defvar log-buffer nil
  "The buffer of log")

(defvar log-file-name "/tmp/easy-emacs.log"
  "The file name of easy-emacs log")

(defmacro log-levels! (&rest args)
  `(progn
     (if (fboundp 'gen_seq)
	 (setq log-levels
	       ',(cl-loop for arg in args
			  collect `(,arg . ,(gen_seq)))))
     ,@(cl-loop for arg in args
		collect `(defmacro ,(intern (concat (symbol-name arg) "!")) (&rest zargs)
			      `(log! ,',arg ,@zargs)))))

(defmacro log-init! (loglvl)
  `(progn
     (let ((zel (alist-get ',loglvl log-levels)))
       (if zel
	   (setq log-level zel)
	 (message "log-init!: unknown log level `%s'" ',loglvl)))))

(defun log-msg (_lvl msg)
  (with-file! log-file-name
	      (format "%s\n" msg)))

(defun set-time-fmt (fmt)
  (setq log-time-fmt fmt))

(defmacro log! (lvl fmt &rest args)
  `(let ((zel (alist-get ',lvl log-levels)))
     (when zel
       (when (<= zel log-level)
	 (let ((msg (format ,(concat "%s  %s  " fmt)
			    (format-time-string log-time-fmt (current-time))
			    ',lvl ,@args)))
	   (log-msg ',lvl msg))))))

(defmacro set-level! (lvl)
  `(let ((zel (alist-get ',lvl log-levels)))
     (when zel
       (setf log-level zel))))

(eval-and-compile
  (log-levels! EMERG ERR WARN INFO DEBUG DEBUG2 DEBUG3))

(provide 'core-log)
