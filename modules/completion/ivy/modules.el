(package!
 ivy
 "Ivy is for quick and easy selection from a list. When Emacs prompts for a string from a list of several possible choices, Ivy springs into action to assist in narrowing and picking the right string from a vast number of choices. "
 ivy)

(package!
 counsel
 "Ivy counsel"
 counsel)

(defun enable-ivy ()
  (ivy-mode 1))

(feature!
 ivy
 "Ivy is for quick and easy selection from a list. When Emacs prompts for a string from a list of several possible choices, Ivy springs into action to assist in narrowing and picking the right string from a vast number of choices. "
 (ivy counsel)
 nil
 enable-ivy
 nil)
