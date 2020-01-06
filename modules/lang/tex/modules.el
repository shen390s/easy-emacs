(scope! tex prog-lang)

(package! :name magic-latex-buffer
	  :docstring "magical syntax highlighting for LaTeX-mode buffers"
	  :pkginfo (magic-latex-buffer :type git
				       :host github
				       :repo
				       "zk-phi/magic-latex-buffer"))

(defun activate-magic-latex ()
  (magic-latex-buffer))

(feature! magic-latex
	  "magical syntax hightlighting for Latex-mode"
	  (magic-latex-buffer)
	  nil
	  activate-magic-latex
	  nil)

(package! :name auctex
	  :docstring "AUCTeX is TeX IDE using Emacs" 
	  :pkginfo auctex)

 	



(defun config-auctex ()
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (load "auctex.el" nil t t)
  (load "auctex-preview.el" nil t t)
  t)

(feature! auctex
	  "TeX IDE using Emacs"
	  (auctex)
	  config-auctex
	  nil
	  nil)
