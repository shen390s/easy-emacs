(defun enable-flymake ()
  (flymake-mode 1))

(defun disable-flymake ()
  (flymake-mode -1))

;; disable flymake as default
(setq flymake-proc-allowed-file-name-masks nil)

(feature! flymake
	  "flymake"
	  nil
	  nil
	  enable-flymake
	  disable-flymake)
