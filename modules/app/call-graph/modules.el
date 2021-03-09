(package! :name call-graph
	  :docstring "Generate call graph for c/c++ functions"
	  :pkginfo (call-graph :type git
			       :host github
			       :repo "emacsmirror/call-graph"))

(package! :name hierarchy
          :docstring "hierarchy"
          :pkginfo (hierarchy :type git
                              :host github
                              :repo "DamienCassou/hierarchy"
                              :post-build ((fix-hierarchy-build)
                                           (message "workarround require error for hierarch"))))

(defun fix-hierarchy-build ()
  (let ((dir (straight--build-dir "hierarchy")))
    (let ((s (shell-command-to-string
              (format "cd %s && rm -Rf *.elc"
                      dir))))
      (message "fix-hierarchy-build: %s" s))))

(defun config-call-graph ()
  t)

(defun enable-call-graph ()
  (require 'call-graph)
  (call-graph))
  
(feature! call-graph
	  "Generate call graph for c/c++ functions"
	  (hierarchy ggtags ivy call-graph)
	  config-call-graph
	  enable-call-graph
	  nil)
    
    
    
