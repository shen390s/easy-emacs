;; -*- lexical-binding: t; -*-
;; core-keybind.el

(defun easy-emacs/define-key (keymap key def &rest bindings)
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
	  def (pop bindings))))

(defmacro bind-major-map (&rest args)
  (let ((map-name (intern (format "%s-map" major-mode))))
    `(progn
       (require 'bind-map)
       (bind-map ,map-name
	 ,@args))))

(defmacro define-key-in-major-map (&rest args)
  (let ((map-name (intern (format "%s-map" major-mode))))
    `(easy-emacs/define-key ,map-name
			    ,@args)))

(provide 'core-keybind)
