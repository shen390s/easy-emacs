;; -*- lexical-binding: t -*-
(package! forge
	     "Work with git forge from comfort of magit"
             (forge :type git
		    :host github
		    :repo "magit/forge"))

(defun activate-forge (scope &optional phase options)
  (require 'magit)
  (require 'forge))

(feature! forge
	     "work with git forge from comfort of magit"
             (magit forge)
             nil
	     nil
             activate-forge)
