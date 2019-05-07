(package!
 golden-ratio
 "Automatic resizing of Emacs windows to the golden ratio "
 (golden-ratio :type git :host github :repo "roman/golden-ratio.el"))

(defun enable-golden-ratio ()
  (golden-ratio-mode 1))

(feature!
 golden-ratio
 "roman/golden-ratio.el"
 (golden-ratio)
 nil
 enable-golden-ratio
 nil)
