(package! :name semantic-stickyfunc-enhance
	  :docstring "Improved version of semantic-stickyfunc-mode that handles parameters on multiple lines "
	  :pkginfo (semantic-stickyfunc-enhance :type git :host github :repo "tuhdo/semantic-stickyfunc-enhance"))

(defun enable-stickyfunc-enhance ()
  (semantic-mode 1)
  (require 'stickyfunc-enhance)
  (semantic-stickyfunc-mode 1))

(feature!
 stickyfunc-enhance
 "Improved version of semantic-stickyfunc-mode that handles parameters on multiple lines"
 (semantic-stickyfunc-enhance)
 nil
 enable-stickyfunc-enhance
 nil)
