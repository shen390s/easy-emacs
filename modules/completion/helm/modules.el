;; -*- lexical-binding: t -*-
(package! helm
	     "Helm is an Emacs framework for incremental completions and narrowing selections. "
	     (helm :type git
		   :host github
		   :repo "emacs-helm/helm"))

(defun activate-helm (scope &optional phase options)
  (require 'helm-config)

  (pcase scope
    ('app (let ((status (plist-get options :status)))
	    (if (and status
		     (>= status 0))
		(progn
		  (global-set-key (kbd "M-x") #'helm-M-x)
		  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
		  (global-set-key (kbd "C-x C-f") #'helm-find-files)
		  (helm-mode 1))
	      (helm-mode -1))))
    (_ t)))

(feature! helm
	     "Helm is an Emacs framework for incremental completion"
	     (helm)
	     nil
	     nil
	     activate-helm)
