(scope! plantuml prog-lang)

(package! plantuml
	  "A major mode for editing PlantUML sources in Emacs"
	  plantuml-mode)

(defun config-plantuml ()
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.plantuml\\'" . lang/plantuml-mode))
    t))

(autoload-r! plantuml-mode
	     (plantuml)
	     "plantuml-mode"
	     t)

(mode! lang/plantuml-mode
       "Emacs mode for plantuml"
       (plantuml)
       config-plantuml
       plantuml-mode)

