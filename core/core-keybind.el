;; -*- lexical-binding: t; -*-
;; core-keybind.el

(defun easy-emacs/define-key (keymap key def &rest bindings)
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
	  def (pop bindings))))

(defmacro define-key-in-major-map (&rest args)
  `(easy-emacs/define-key evil-normal-state-local-map
			  ,@args))

(defun mk-mode-keybinds (mode keybinds)
  (DEBUG! "mk-mode-keybinds %s %s"
	  mode keybinds)
  (when keybinds
    `(evil-leader/set-key-for-mode
       ',mode
       ,@keybinds)))

(provide 'core-keybind)
