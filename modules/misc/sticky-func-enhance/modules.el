(package!
 semantic-stickyfunc-enhance
 "Improved version of semantic-stickyfunc-mode that handles parameters on multiple lines "
 (semantic-stickyfunc-enhance :type git :host github :repo "tuhdo/semantic-stickyfunc-enhance"))

(defun enable-stickyfunc-enhance ()
  (semantic-mode 1)
  (require 'stickyfunc-enhance))

(feature!
 stickyfunc-enhance
 "Improved version of semantic-stickyfunc-mode that handles parameters on multiple lines"
 (semantic-stickyfunc-enhance)
 nil
 enable-stickyfunc-enhance
 nil)
