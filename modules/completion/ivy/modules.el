(package! :name ivy
	  :docstring "Ivy is for quick and easy selection from a list. When Emacs prompts for a string from a list of several possible choices, Ivy springs into action to assist in narrowing and picking the right string from a vast number of choices. "
	  :pkginfo ivy)

(package! :name counsel
	  :docstring "Ivy counsel"
	  :pkginfo counsel)

(package! :name swiper
	  :docstring "Ivy swiper"
	  :pkginfo swiper)

(package! :name ivy-posframe
	  :docstring "Using posframe to show Ivy"
          :pkginfo (ivy-posframe :type git
				 :host github
				 :repo "emacsmirror/ivy-posframe"))

;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))

(defun enable-ivy ()
  (require 'ivy-posframe)
  (progn    
    (ivy-mode 1)
    (ivy-posframe-mode 1)))

(feature! ivy
	  "Ivy is for quick and easy selection from a list. When Emacs prompts for a string from a list of several possible choices, Ivy springs into action to assist in narrowing and picking the right string from a vast number of choices. "
	  (ivy counsel swiper ivy-posframe)
	  nil
	  enable-ivy
	  nil)
