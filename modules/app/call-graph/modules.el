;; -*- lexical-binding: t -*-

(package! call-graph
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

(defun config-call-graph (scope &optional phase options)
  (DEBUG! "config-call-graph scope %s phase %s options %s"
	  scope phase options)
  (pcase phase
    (:check (after-activate!
	     evil
	     (progn
	       (require 'evil-leader)
	       (require 'call-graph)
	       (evil-leader/set-key-for-mode (get-mode-from-options options)
		 (kbd "mcg") 'call-graph)
	       (evil-leader/set-key-for-mode 'call-graph-mode
		 (kbd "ce") 'cg-widget-expand-all
		 (kbd "cc") 'cg-widget-collapse-all
		 (kbd "c+") 'cg-expand
		 (kbd "c_") 'cg-collapse
		 (kbd "co") 'cg-goto-file-at-point
		 (kbd "cg") 'cg-at-point))))
    (_ t)))

(defun activate-call-graph (scope &optional phase options)
  (require 'call-graph)
  (pcase scope
    ('modes (progn
	      (let ((status (plist-get options :status)))
		(when (and status
			   (>= status 0))
		  (call-graph)))))
    (_ t)))
  
(feature! call-graph
	     "Generate call graph for c/c++ functions"
	     (hierarchy ggtags ivy call-graph)
	     config-call-graph
	     nil
	     activate-call-graph)
    
    
    
