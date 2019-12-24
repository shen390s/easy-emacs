(package! :name call-graph
	  :docstring "Generate call graph for c/c++ functions"
	  :pkginfo (call-graph :type git
			       :host github
			       :repo "emacsmirror/call-graph"))

(defun config-call-graph ()
  t)

(defun enable-call-graph ()
  (require 'call-graph)
  (call-graph))
  
(feature! call-graph
	  "Generate call graph for c/c++ functions"
	  (call-graph ggtags ivy)
	  config-call-graph
	  enable-call-graph
	  nil)
    
    
    
