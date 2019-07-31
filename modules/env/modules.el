(defmacro activate-environ (&rest vars)
  (apply 'set-vars vars))
  
(feature! environ
	  "setting of environment"
          nil
          nil
          activate-environ
          nil)
