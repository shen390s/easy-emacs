(scope! plantuml prog-lang)

(package! :name plantuml
	  :docstring "A major mode for editing PlantUML sources in Emacs"
	  :pkginfo plantuml-mode)

(defun config-plantuml ()
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.plantuml\\'" . lang/plantuml-mode))
    t))

(autoload-r! plantuml-mode
	     (plantuml)
	     "plantuml-mode"
	     t)

(rmode! lang/plantuml-mode
	"Emacs mode for plantuml"
	(plantuml)
	config-plantuml
	plantuml-mode)

