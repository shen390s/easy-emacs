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

(provide 'core-lib)
