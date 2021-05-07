(package-ex! call-graph
	     "Generate call graph for c/c++ functions"
	     (call-graph :type git
			 :host github
			 :repo "emacsmirror/call-graph"))

(package! hierarchy
          "hierarchy"
          (hierarchy :type git
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

(defun activate-call-graph (scope &optional phase options)
  (require 'call-graph)
  (when (> (plist-get options :status) 0)
      (call-graph)))
  
(feature-ex! call-graph
	     "Generate call graph for c/c++ functions"
	     (hierarchy ggtags ivy call-graph)
	     nil
	     nil
	     enable-call-graph)
    
    
    
