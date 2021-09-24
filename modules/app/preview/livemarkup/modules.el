;; -*- lexical-binding: t -*-
(package! emacs-livemarkup
	     "live preview for org and asciidoc"
	     (emacs-livemarkup :type git
			       :host github
			       :repo "shen390s/emacs-livemarkup"))

(defun livemarkup-config (scope &optional phase options)
  (pcase phase
    ('primary
     (progn
       (setq livemarkup-output-directory nil
	     livemarkup-close-buffer-delete-temp-files t
	     livemarkup-refresh-interval 3)))
    (_ t)))

(defun activate-livemarkup (scope &optional phase options)
  (DEBUG! "activate-livemarkup scope %s phase %s options %s"
	  scope phase options)
  (require 'livemarkup)
  (let ((file-name (buffer-file-name)))
    (let ((ext-name (file-name-extension file-name)))
      (cond
       ((string= ext-name "org") (livemarkup-track-org))
       ((string= ext-name "adoc") (livemarkup-track-asciidoc))
       (t t)))))

(feature! livemarkup
	  "Emacs Editor Livemarkup"
	  (emacs-livemarkup)
	  livemarkup-config
	  nil
	  activate-livemarkup)

