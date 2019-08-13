;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'core-lib)

(defvar log-level 0 "log level")  

(defvar log-levels nil
  "Pre-defined log levels")

(defvar log-time-fmt "%m/%d/%Y %r"
  "Default log format of time")

(defvar log-buffer-name "*EasyEmacs Logs*"
  "The buffer name of log buffer")

(defvar log-buffer nil
  "The buffer of log")

(defmacro log-define-levels! (&rest args)
  (if (fboundp 'gen_seq)
      `(setq log-levels
	     ',(cl-loop for arg in args
			collect `(,arg . ,(gen_seq))))))

(defmacro log-init! (loglvl)
  `(progn
     (let ((zel (assoc ',loglvl log-levels)))
       (if zel
	   (setq log-level (cdr zel))
	 (message "log-init!: unknown log level `%s'" ',loglvl)))))

(defun log-msg (_lvl msg)
  (unless log-buffer
    (setq log-buffer (get-buffer-create log-buffer-name)))
  (with-current-buffer log-buffer
    (save-excursion
      (goto-char (point-max))
      (insert (concat msg "\n")))))

(defun set-time-fmt (fmt)
  (setq log-time-fmt fmt))

(defmacro log! (lvl fmt &rest args)
  `(let ((zel (assoc ',lvl log-levels)))
     (when (<= (cdr zel) log-level)
      (let ((msg (format ,(concat "%s  %s  " fmt)
			 (format-time-string log-time-fmt (current-time))
			 ',lvl ,@args)))
	(log-msg ',lvl msg)))))

(provide 'core-log)
