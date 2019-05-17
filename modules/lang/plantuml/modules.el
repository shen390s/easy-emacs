(package!
 plantuml
 "A major mode for editing PlantUML sources in Emacs"
 plantuml-mode)

(defun config-plantuml ()
  (add-to-list 'auto-mode-alist
	       '("\\.plantuml\\'" . plantuml-mode)))

(feature!
 plantuml
 "A major mode for editing PlantUML sources in Emacs"
 (plantuml)
 config-plantuml
 nil
 nil)
