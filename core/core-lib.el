;; -*- lexical-binding: t -*-
;; enable lexical scope

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
      
(defmacro when-bind! (var exp &rest body)
  `(let ((,var ,exp))
     (when ,var
       ,@body)))

(defmacro log-init (fmt time-fmt)
  `())

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
  
(provide 'core-lib)
