(package-ex! ivy
	  "Ivy is for quick and easy selection from a list. 
When Emacs prompts for a string from a list of several possible 
choices, Ivy springs into action to assist in narrowing and 
picking the right string from a vast number of choices. "
	  ivy)

(package-ex! counsel
	     "Ivy counsel"
	     counsel)

(package-ex! swiper
	     "Ivy swiper"
	     swiper)

(package-ex!  ivy-posframe
	      "Using posframe to show Ivy"
              (ivy-posframe :type git
			    :host github
			    :repo "emacsmirror/ivy-posframe"))

;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))

(defun ivy-activate (scope &optional phase options)
  (require 'ivy-posframe)
  (ivy-mode 1)
  (ivy-posframe-mode 1))

(feature-ex! ivy
	     "Ivy is for quick and easy selection from a list. 
When Emacs prompts for a string from a list of several possible 
choices, Ivy springs into action to assist in narrowing and 
picking the right string from a vast number of choices. "
	     (ivy counsel swiper ivy-posframe)
	     nil
	     nil
	     ivy-activate)
