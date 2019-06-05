;; -*- lexical-binding: t; -*-
;; core-keybind.el

(defun easy-emacs/define-key (keymap key def &rest bindings)
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindins)
	  def (pop bindings))))

(provide 'core-keybind)
