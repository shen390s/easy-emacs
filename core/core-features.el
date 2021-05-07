;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'core-lib)
(require 'core-log)
(require 'core-modules)

(defvar feature-key-args '(:activate :deactivate :when)
  "Key args for feature")

(defvar actived-modes nil
  "A list of actived modes")

(defun config-mode (m)
  (let ((zmode (get-mode m)))
    (if zmode
	(Feature/configure zmode)
      t)))

;; create global scope
;;

(provide 'core-features)
