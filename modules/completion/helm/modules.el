(package! :name helm
	  :docstring "Helm is an Emacs framework for incremental completions and narrowing selections. "
	  :pkginfo (helm :type git
			 :host github
			 :repo "emacs-helm/helm"))

(defun config-helm ()
  (require 'helm-config)
  (progn
    (global-set-key (kbd "M-x") #'helm-M-x)
    (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
    (global-set-key (kbd "C-x C-f") #'helm-find-files)))

(defun enable-helm ()
  (helm-mode))

(defun disable-helm ()
  t)

(feature! helm
	  "Helm is an Emacs framework for incremental completion"
	  (helm)
	  config-helm
	  enable-helm
	  disable-helm)
