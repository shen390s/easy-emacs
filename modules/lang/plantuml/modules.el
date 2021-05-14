
(package-ex! plantuml
	     "A major mode for editing PlantUML sources in Emacs"
	     plantuml-mode)

(autoload-r! plantuml-mode
	     (plantuml)
	     "plantuml-mode"
	     t)

(add-auto-features "plantuml-mode" 'plantuml)

(defun config-plantuml (scope &optional phase options)
  t)

(feature-ex! plantuml
	     "Feature for plantuml mode"
	     (plantuml)
	     config-plantuml
	     nil
	     nil)

