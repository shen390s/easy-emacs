;; -*- lexical-binding: t -*-
(package! parinfer
	     "Parinfer is a proof-of-concept editor mode for Lisp programming languages.
 It will infer some changes to keep Parens and Indentation inline with one another. "
	     (parinfer :type git
		       :host github
		       :repo "DogLooksGood/parinfer-mode"))

(package! iedit
	     "Edit multiple regions in the same way simultaneously"
	     (iedit :type git
		    :host github
		    :repo "victorhge/iedit"))

(defun config-parinfer (scope &optional phase options)
  (pcase phase
    (:check
     (progn
       (setq parinfer-extensions (list 'defaults
				       'pretty-parens
				       'lispy
				       'paredit
				       'smart-yank))
       (after-activate! evil 
			(progn
			  (setq parinfer-extensions
				(append parinfer-extensions '(evil)))))))
    (_ t)))

(defun activate-parinfer (scope &optional phase options)
  (require 'lispy)
  (require 'paredit)
  (require 'parinfer)
  (require 'parinfer-ext)

  (let ((status (plist-get options
			   :status)))
    (if (and status
	       (>= status 0))
	(parinfer-mode 1)
      (parinfer-mode -1))))

(feature! parinfer
	     "Parinfer"
	     (parinfer iedit lispy paredit)
	     config-parinfer
	     nil
	     activate-parinfer)

(defun activate-iedit (scope &optional phase options)
  (DEBUG! "activate-iedit scope %s phase %s options %s"
	  scope phase options)
  (require 'iedit)
  t)


(feature! iedit
	     "Edit multiple regions in the same way simultaneouesly"
	     (iedit)
	     nil
	     nil
	     activate-iedit)
