;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'core-lib)
(require 'core-log)
(require 'core-modules)

(defvar feature-key-args '(:activate :deactivate :when)
  "Key args for feature")

(defun parse-feature (feature)
  (let ((f-name (format "%s" feature)))
    (pcase (substring f-name 0 1)
      ("+" (list (intern (substring f-name 1)) 1))
      ("-" (list (intern (substring f-name 1)) -1))
      (_ (list (intern f-name) 0)))))

(defun unparse-feature (feature)
  (pcase (cadr feature)
    (1 (intern (format "+%s" (car feature))))
    (-1 (intern (format "-%s" (car feature))))
    (0  (intern (car feature)))))

(defun parse-features (features)
  (cl-loop for feature in features
	   collect (parse-feature feature)))

(defun unparse-features (features)
  (cl-loop for feature in features
	   collect (unparse-feature feature)))

(defun has-feature? (features feature)
  (let ((found nil))
    (cl-loop for feat in features
	     until (when (string= (car feat)
				  (car feature))
		     (setq found t)))
  found))

(provide 'core-features)
