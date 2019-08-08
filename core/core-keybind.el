;; -*- lexical-binding: t; -*-
;; core-keybind.el

(defun easy-emacs/define-key (keymap key def &rest bindings)
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
	  def (pop bindings))))

(defmacro bind-major-map (&rest args)
  `(progn
     (require 'bind-map)
     (bind-map (current-local-map)
	       ,@args)))

(defmacro define-key-in-major-map (&rest args)
  `(easy-emacs/define-key (current-local-map)
			  ,@args))

(provide 'core-keybind)
