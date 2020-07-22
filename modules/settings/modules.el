(defvar use-polymode nil
  "enable poly mode")

(defvar my-coding-system 'utf-8
  "coding system setting")

(defun setup-coding-system (cs)
  (prefer-coding-system cs)
  (set-default-coding-systems cs)
  (set-terminal-coding-system cs)
  (set-keyboard-coding-system cs)
  (if (boundp 'buffer-file-coding-system)
      (setq-default buffer-file-coding-system cs)
    (setq default-buffer-file-coding-system cs))

  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT
					    STRING)))

(defmacro activate-settings (&rest vars)
  `(progn
     (set-vars ,@vars)
     (setup-coding-system my-coding-system)))
  

(feature! settings
	  "setting of settingsment"
          nil
          nil
          activate-settings
          nil)
