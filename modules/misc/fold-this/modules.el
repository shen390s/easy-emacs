(package! fold-this
	  "Fold the active region"
          (fold-this :type git
		     :host github
                     :repo "magnars/fold-this.el"))

(defun config-fold-this ()
  t)

(defun activate-fold-this ()
  (require 'fold-this)
  (fold-this-mode 1))

(feature! fold-this
	  "fold the actived region"
          (fold-this)
          config-fold-this
          activate-fold-this
          nil)
                     
