;; trace how flymake has been called
(defun trace-flymake-mode (origin-fun &rest args)
  (debug)
  (apply origin-fun args))
  
(defun config-flymake ()
  (progn
    ;; (advice-add 'flymake-mode
    ;; 		:around
    ;; 		#'trace-flymake-mode)
    t))
              
(defun enable-flymake ()
  (flymake-mode 1))

(defun disable-flymake ()
  ;;(message "disable-flymake for buffer %s" (buffer-name))
  (flymake-mode -1))

;; disable flymake as default
(setq flymake-proc-allowed-file-name-masks nil)

(feature! flymake
	  "flymake"
	  nil
	  config-flymake
	  enable-flymake
	  disable-flymake)
