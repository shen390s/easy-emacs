(defun enable-linumber ()
  (linum-mode 1))

(feature!
 linum
 "Show line number"
 nil
 nil
 enable-linumber
 nil)
