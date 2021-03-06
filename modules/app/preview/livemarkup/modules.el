(package! :name emacs-livemarkup
	  :docstring "live preview for org and asciidoc"
	  :pkginfo (emacs-livemarkup :type git
			             :host github
			             :repo "shen390s/emacs-livemarkup"))

(defun livemarkup-config ()
  (setq livemarkup-output-directory nil
	livemarkup-close-buffer-delete-temp-files t
	livemarkup-refresh-interval 3)
  t)

(defun activate-livemarkup ()
  (require 'livemarkup)
  (let ((file-name (buffer-file-name)))
    (let ((ext-name (file-name-extension file-name)))
      (cond
       ((string= ext-name "org") (livemarkup-track-org))
       ((string= ext-name "adoc") (livemarkup-track-asciidoc))
       (t t)))))

(defun deactivate-livemarkup ()
  (livemarkup-untrack))

(feature! livemarkup
	  "Emacs Editor Livemarkup"
	  (emacs-livemarkup)
	  livemarkup-config
	  activate-livemarkup
	  deactivate-livemarkup)

