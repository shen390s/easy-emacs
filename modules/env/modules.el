(defmacro activate-environ (&rest vars)
  `(set-vars ,@vars))

  
(feature! environ
	  "setting of environment"
          nil
          nil
          activate-environ
          nil)
