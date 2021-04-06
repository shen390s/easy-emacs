
(package! :name mermaid-mode
	  :docstring "A major Emacs mode for edit mermaid document"
	  :pkginfo (mermaid-mode :type git
				 :host github
				 :repo "shen390s/mermaid-mode"))

(defun config-mermaid ()
  (DEBUG! "configuring mermaid mode...")
  (progn
    (add-to-list 'auto-mode-alist
		 '("\\.mmd\\'" . lang/mermaid-mode))
    (when (feature-enabled 'evil)
      (require 'bind-map)
      (bind-map my-mermaid-leader-map
       	:evil-keys ("m")
       	:evil-states (normal visual)
       	:major-modes (mermaid-mode))

      (bind-map-set-keys my-mermaid-leader-map
       	"c" 'mermaid-compile
       	"v" 'mermaid-view))
    t))

(defun activate-mermaid ()
  (mermaid-mode)
  ;; FIXME: activate my key bindings
  (setf my-mermaid-leader-map-active t))

(autoload-r! mermaid-mode
	     (mermaid-mode)
	     "mermaid"
	     t)

(rmode! lang/mermaid-mode
	"Emacs mode for mermaid"
	(mermaid-mode)
	config-mermaid
	activate-mermaid)

