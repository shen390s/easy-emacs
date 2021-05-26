;; -*- lexical-binding: t -*-
;; enable lexical scope

(defun show-paren-line ()
  ;; we will call `blink-matching-open` ourselves...
  (remove-hook 'post-self-insert-hook
	       #'blink-paren-post-self-insert-function)
  ;; this still needs to be set for `blink-matching-open` to work
  (setq blink-matching-paren 'show)

  (let ((ov nil)) ; keep track of the overlay
    (advice-add
     #'show-paren-function
     :after
     (defun show-paren--off-screen+ (&rest _args)
       "Display matching line for off-screen paren."
       (when (overlayp ov)
	 (delete-overlay ov))
       ;; check if it's appropriate to show match info,
       ;; see `blink-paren-post-self-insert-function'
       (when (and (overlay-buffer show-paren--overlay)
		  (not (or cursor-in-echo-area
			   executing-kbd-macro
			   noninteractive
			   (minibufferp)
			   this-command))
		  (and (not (bobp))
		       (memq (char-syntax (char-before)) '(?\) ?\$)))
		  (= 1 (logand 1 (- (point)
				    (save-excursion
				      (forward-char -1)
				      (skip-syntax-backward "/\\")
				      (point))))))
	 ;; rebind `minibuffer-message' called by
	 ;; `blink-matching-open' to handle the overlay display
	 (cl-letf (((symbol-function #'minibuffer-message)
		    (lambda (msg &rest args)
		      (let ((msg (apply #'format-message msg args)))
			(setq ov (display-line-overlay+
				  (window-start) msg ))))))
	   (blink-matching-open))))))
  (setq show-paren-style 'paren
	show-paren-delay 0.03
	show-paren-highlight-openparen t
	show-paren-when-point-inside-paren nil
	show-paren-when-point-in-periphery t)
  (show-paren-mode 1))
          
(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:inherit default :inherit highlight)))
    ol))

(defun activate-show-paren-line (scope &optional phase options)
  (let ((status (plist-get  options :status)))
    (when (and status
	       (>= status 0))
      (show-paren-line))))

(feature! show-paren-line 
	     "Show matching lines when parentheses go off-screen"
             nil
             nil
	     nil
             active-show-paren-line)
