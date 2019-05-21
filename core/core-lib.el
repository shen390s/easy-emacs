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

(defmacro when-bind! (var exp &rest body)
  `(let ((,var ,exp))
     (when ,var
       ,@body)))

(provide 'core-lib)
