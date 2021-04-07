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

(defun filt-key-args (collected-args keys args)
  (if (null args)
      (reverse collected-args)
    (let ((x (car args))
	  (remain (cdr args)))
      (cond
       ((member x keys)
	(if (null remain)
	    (reverse collected-args)
	  (filt-key-args collected-args keys (cdr remain))))
       (t (filt-key-args (push x collected-args) keys remain))))))

;; -feature to disable feature explicit
(defun parse-feature-name (n)
  (let ((svalue (symbol-name n)))
    (if (string= (substring svalue 0 1) "-")
	(cons t (intern (substring svalue 1)))
      (cons nil n))))

(defun extract-feature-name (feature)
  (if (consp feature)
      (cons nil (car feature))
    (parse-feature-name feature)))

(defun extract-feature-args (feature)
  (if (consp feature)
      (filt-key-args nil feature-key-args
		     (cdr feature))
    nil))

(defun extract-hook-action (feature tag subtag)
  (if (consp feature)
      (cl-getf (cl-getf (cdr feature) tag) subtag)
    nil))

(defun extract-active-condition (feature)
  (if (consp feature)
      (cl-getf (cdr feature) :when t)
    t))

(defun parse-feature (feature)
  (pcase (extract-feature-name feature)
    (`(,is-disabled . ,feature-name)
     (let ((pre-activate-action (extract-hook-action feature
						     :activate
						     :pre))
	   (post-activate-action (extract-hook-action feature
						      :activate
						      :post))
	   (pre-deactivate-action (extract-hook-action feature
						       :deactivate
						       :pre))
	   (post-deactivate-action (extract-hook-action feature
							:deactivate
							:post))
	   (active-condition (extract-active-condition feature))
	   (args (extract-feature-args feature)))
       (list :name feature-name
	     :disabled is-disabled
	     :active-condition active-condition
	     :pre-activate-action pre-activate-action
	     :post-activate-action post-activate-action
	     :pre-deactivate-action pre-deactivate-action
	     :post-deactivate-action post-deactivate-action
	     :args args)))))

(defun mk-function-call (fn args)
  (if fn
      `(,fn ,@args)
    `(dummy-fn)))

(defun mk-activate (condition pre-action fn post-action args)
  `(when ,condition
     ,@(mk-action pre-action)
     (let ((result ,(mk-function-call fn args)))
       ,@(mk-action post-action)
       result)))

(defun get-feature-activate-fn (feature)
  (let ((xfeature (get-feature feature)))
    (let ((fn (oref xfeature on-fn)))
      (if fn
	  fn
	#'dummy-fn))))

(defun get-feature-deactivate-fn (feature)
  (let ((xfeature (get-feature feature)))
    (let ((fn (oref xfeature off-fn)))
      (if fn
	  fn
	#'dummy-fn))))

(defun activate-feature (feature)
  (mk-activate (plist-get feature :active-condition)
	       (plist-get feature :pre-activate-action)
	       (get-feature-activate-fn (plist-get feature :name))
	       (plist-get feature :post-activate-action)
	       (plist-get feature :args)))

(defun disable-feature (feature)
  (mk-activate t
	       nil
	       (get-feature-deactivate-fn (plist-get feature :name))
	       nil
	       nil))

(defun deactivate-feature (feature)
  (mk-activate  (plist-get feature :active-condition)
	        (plist-get feature :pre-deactivate-action)	
	        (get-feature-activate-fn (plist-get feature :name))
	        (plist-get feature :post-deactivate-action)
	        nil))


(defun config-mode (m)
  (let ((zmode (get-mode m)))
    (if zmode
	(Feature/configure zmode)
      t)))

;; create global scope
;;

(provide 'core-features)
