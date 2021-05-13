(package! :name adoc-mode
	  :docstring "A major Emacs mode for edit ascii-doctor document"
	  :pkginfo (adoc-mode :type git
			      :host github
			      :repo "shen390s/adoc-mode"))

(defun adoc-tune-faces ()
  (progn
    (let ((face-height (/ (x-display-pixel-width) 16)))
      (cl-loop for face in '(adoc-align adoc-anchor adoc-generic
					adoc-monospace adoc-strong
					adoc-emphasis adoc-superscript
					adoc-subscript
					adoc-secondary-text
					adoc-replacement
					adoc-complex-replacement
					adoc-list-item adoc-table-del
					adoc-reference adoc-delimiter
					adoc-hide-delimiter adoc-comment
					adoc-warning adoc-preprocessor)
	       do (set-face-attribute face nil :height face-height))))
  t)

(defun ascii-doctor-setup ()
  (adoc-tune-faces))

(defun config-adoc ()
  (add-to-list 'auto-mode-alist
	       '("\\.adoc\\'" . lang/adoc-mode))
  t)

(autoload-r! adoc-mode
	     (adoc-mode)
	     "adoc-mode"
	     t)

(rmode! lang/adoc-mode
	"Emacs mode for ascii-doctor"
	(adoc-mode)
	config-adoc
	adoc-mode)

