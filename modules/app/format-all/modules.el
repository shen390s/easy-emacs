(package! format-all
	  "Auto-format c,c++,JS,Python and more"
          (format-all :type git
		      :host github
                      :repo "emacsmirror/format-all"))

(defun activate-format-all ()
  (require 'format-all)
  (format-all-mode 1))
  
(feature! format-all
	  "Auto-format c,c++,JS,Python and more"
          (format-all)
          nil
          activate-format-all
          nil)                      
