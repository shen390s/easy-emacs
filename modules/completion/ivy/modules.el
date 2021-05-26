(package! ivy
	  "Ivy is for quick and easy selection from a list. 
When Emacs prompts for a string from a list of several possible 
choices, Ivy springs into action to assist in narrowing and 
picking the right string from a vast number of choices. "
	  ivy)

(package! counsel
	     "Ivy counsel"
	     counsel)

(package! swiper
	     "Ivy swiper"
	     swiper)

(package!  ivy-posframe
	      "Using posframe to show Ivy"
              (ivy-posframe :type git
			    :host github
			    :repo "emacsmirror/ivy-posframe"))

;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))

(defun ivy-activate (scope &optional phase options)
  (require 'ivy-posframe)
  (ivy-mode 1)
  (ivy-posframe-mode 1))

(feature! ivy
	     "Ivy is for quick and easy selection from a list. 
When Emacs prompts for a string from a list of several possible 
choices, Ivy springs into action to assist in narrowing and 
picking the right string from a vast number of choices. "
	     (ivy counsel swiper ivy-posframe)
	     nil
	     nil
	     ivy-activate)
