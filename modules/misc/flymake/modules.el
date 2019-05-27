(defun enable-flymake ()
  (flymake-mode 1))

(defun disable-flymake ()
  (flymake-mode -1))

(feature! flymake
	  "flymake"
	  nil
	  nil
	  enable-flymake
	  disable-flymake)
