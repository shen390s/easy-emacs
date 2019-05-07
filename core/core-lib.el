;; -*- lexical-binding: t -*-
;; enable lexical scope

(defun collect-lists (acc lists)
  (if (null lists)
      acc
    (collect-lists (append acc (car lists))
		   (cdr lists))))

(provide 'core-lib)
