;; -*- lexical-binding: t -*-

(package! magic-latex-buffer
	     "magical syntax highlighting for LaTeX-mode buffers"
	     (magic-latex-buffer :type git
				 :host github
				 :repo
				 "zk-phi/magic-latex-buffer"))

(defun activate-magic-latex (scope &optional phase options)
  (require 'magic-latex-buffer)
  (let ((status (plist-get options :status)))
    (when (and status
	       (>= status 0))
      (magic-latex-buffer))))

(feature! magic-latex
	  "magical syntax hightlighting for Latex-mode"
	  (magic-latex-buffer)
	  nil
	  nil
	  activate-magic-latex)

(package! auctex
	     "AUCTeX is TeX IDE using Emacs" 
	     auctex)

(defun config-auctex (scope &optional phase options)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(defun activate-auctex (scope &optional phase options)
  (load "auctex.el" nil t t)
  (load "auctex-preview.el" nil t t)
  t)

(feature! auctex
	     "TeX IDE using Emacs"
	     (auctex)
	     config-auctex
	     nil
	     activate-auctex)
