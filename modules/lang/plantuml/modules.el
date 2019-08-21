(package! plantuml
	  "A major mode for editing PlantUML sources in Emacs"
	  plantuml-mode)

(defun config-plantuml ()
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.plantuml\\'" . plantuml-mode))
    (attach! plantuml plantuml-mode)
    t))

(feature! plantuml
	  "A major mode for editing PlantUML sources in Emacs"
	  (plantuml)
	  config-plantuml
	  nil
	  nil)
