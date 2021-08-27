;; -*- lexical-binding: t -*-
(package! best_git_mirrors
	  "best mirror of github for china users"
	  (best_git_mirrors :type git
			    :host github
			    :repo "shen390s/git_best_mirrors"
			    :no-built t))

(autoload-r! git_best_mirrors
	     (best_git_mirrors)
	     "git_best_mirrors"
	     nil)

(defun do-config-best-git-mirrors ()
  (let ((git-best-mirror-tool-dir
	 (concat easy-emacs-config-dir
		 "/straight/repos/git_best_mirrors/bin")))
    (progn
      (setq exec-path
	    (cons git-best-mirror-tool-dir
		  exec-path))
      (prepend-env git-best-mirror-tool-dir "PATH")
      (prepend-env git-best-mirror-tool-dir "EMACSPATH"))))

(defun config-best_git_mirrors (scope &optional phase options)
  (pcase scope
    ('core (let ((status (plist-get options :status)))
	     (when (and status
			(>= status 0)
			(eq phase :pre-check))
	       (git_best_mirrors)
	       (do-config-best-git-mirrors))))
    (_ t)))


(feature! best_git_mirrors
	  "use best mirror of github for china users"
	  (best_git_mirrors)
	  config-best_git_mirrors
	  nil
	  nil)
