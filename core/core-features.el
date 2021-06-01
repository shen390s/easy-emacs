;; -*- lexical-binding: t -*-
;; enable lexical scope

(require 'cl-lib)
(require 'subr-x)
(require 'core-lib)
(require 'core-log)
(require 'core-modules)

(defvar feature-key-args '(:activate :deactivate :when)
  "Key args for feature")

(provide 'core-features)
