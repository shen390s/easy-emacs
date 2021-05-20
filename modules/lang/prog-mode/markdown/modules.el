
(package-ex! markdown-mode
	     "A major Emacs mode for edit markdown document"
	     markdown-mode)

(package-ex! vmd-mode
	     "Snappy Markdown preview minor mode for emacs"
	     (vmd-mode :type git
		       :host github
		       :repo "blak3mill3r/vmd-mode"))

(autoload-r! markdown-mode
	     (markdown-mode)
	     "markdown-mode"
	     t)

(autoload-r! gfm-mode
	     (markdown-mode)
	     "markdown-mode"
	     t)

(defun enable-vmd ()
  (let ((file (buffer-file-name)))
    (unless (file-exists-p file)
      (save-buffer)))
  (vmd-mode))

(defun deactivate-vmd ()
  (unless vmd-process
    (delete-process vmd-process)))

(defun activate-vmd (scope &optional phase options)
  (DEBUG! "activate-vmd scope %s phase %s options %s"
	  scope phase options)
  (require 'vmd-mode)
  (let ((status (plist-get options :status)))
    (when status
      (if (>= status 0)
	  (enable-vmd)
	(deactivate-vmd)))))

(feature-ex! vmd
	     "Snappy Markdown preview minor mode for emacs"
	     (vmd-mode)
	     nil
	     nil
	     activate-vmd)
