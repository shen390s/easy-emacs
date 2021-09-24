;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'pp)
(require 'core-lib)
(require 'core-log)
(require 'core-hooks)
(require 'core-modules)

(defclass Scope ()
  ((name :initarg :name)
   (configs :initarg :configs
	    :initform nil)
   (code :initform nil))
  "Scope definition")

(cl-defmethod Scope/init ((scope Scope))
  (DEBUG! "Scope/init scope = \n%s"
	  scope)
  t)

(cl-defmethod Scope/Code:get ((scope Scope) action phase)
  (let ((key (intern (format ":%s-%s" action phase))))
    (with-slots (code) scope
      (DEBUG! "Scope/Code:get action %s phase %s \n scope =\n%s \ncode=\n%s"
	      action phase
	      (Scope/to-string scope)
	      (pp-to-string code))
      (let ((c (plist-get code key)))
	(DEBUG! "Scope/Code:get () =\n%s"
		(pp-to-string c))
	c))))

(cl-defmethod Scope/to-string ((scope Scope))
  (with-slots (name configs code) scope
    (format "\n\nName: %s \nConfigs:\n %s \nCode:\n %s\n"
	    name (pp-to-string configs)
	    (pp-to-string code))))

(defclass Initial-Scope (Scope)
  ()
  "Scope for initialization")

(cl-defmethod Scope/init ((scope Initial-Scope))
  (with-slots (configs code) scope
    (setf code
	  (plist-put nil
		     :prepare-before
		     `,@configs))))

(defclass Normalized-Scope (Scope)
  ((n-configs :initform nil))
  "Scope with normalized configs")

(cl-defmethod Scope/to-string ((scope Normalized-Scope))
  (let ((s (cl-call-next-method)))
    (with-slots (n-configs) scope
      (format "%s\nn-configs:\n%s"
	      s
	      (pp-to-string n-configs)))))

(defclass Feature-Scope (Normalized-Scope)
  ()
  "Scope with list of features")

(defun normalize-feature-config (config)
  (DEBUG! "normalize-feature-config config=\n%s"
	  (pp-to-string config))
  (let ((fn (car config))
	(config1 (normalize-options (cdr config)))
	(n-config nil)
	(options nil))

    (DEBUG! "normalize-feature-config config1 =\n%s"
	    (pp-to-string config1))
    (setq n-config
	  (plist-put n-config
		     :name fn))

    (setq options
	  (plist-get config1 :options))
    
    (cl-loop for key in (filt-out-non-keywords config1)
	     do (pcase key
		  ;; exclude :options
		  ;;  before/after-activate
		  (:options t)
		  (:after-activate t)
		  (:before-activate t)
		  (_ (setq options
			   (plist-put options
				      key
				      (plist-get config1
						 key))))))
    (setq n-config
	  (plist-put n-config
		     :options options))

    (cl-loop for key in '(:before-activate :after-activate)
	     do (let ((v (plist-get config1 key)))
		  (when v
		    (setq n-config
			  (plist-put n-config
				     key
				     v)))))
    n-config))

(defun normalize-mode-config (config)
  (DEBUG! "normalize-mode-config config =\n%s"
	  (pp-to-string config))
  (let ((cn (car config))
	(config1 (normalize-options (cdr config)))
	(n-config nil))

    (DEBUG! "normalize-mode-config config1 =\n%s"
	    (pp-to-string config1))
    (setq n-config
	  (plist-put nil
		     :name cn))
    (cl-loop for key in (filt-out-non-keywords config1)
	     do (let ((v (plist-get config1 key)))
		  (let ((nv (pcase key
			      ;; single inherit
			      (:inherit (car v))

			      ;; normalize features
			      (:features (parse-features v))
			      (_ v))))
		    (when nv
		      (setq n-config
			    (plist-put n-config
				       key
				       nv))))))
    
    (DEBUG! "normalize-mode-config n-config = \n%s"
	    (pp-to-string n-config))
    n-config))

(defun mk-code/:feature-config (config scope action phase)
  (let ((options (plist-put (plist-get config :options)
			    :status 1))
	(fn (plist-get config :name)))
    (pcase action
      ('activate `((install-packages (invoke-feature ',fn 'pkglist
						     ',scope 'ignore
						     ',options))
		   ,@(plist-get config :before-activate)
		   (invoke-feature ',fn ',action
				   ',scope 'ignore
				   ',options)
		   ,@(plist-get config :after-activate)))
      (_ `((invoke-feature ',fn ',action
			   ',scope ',phase
			   ',options))))))

(cl-defmethod Scope/init ((scope Feature-Scope))
  (with-slots (configs n-configs code name) scope
    (setf n-configs
	  (cl-loop for config in configs
		   collect (normalize-feature-config config)))

    (let ((c1 nil))
      (cl-loop for action in '(prepare configure)
	       do (cl-loop for phase in '(before primary after)
			   do (setf c1
				    (plist-put c1
					       (intern (format ":%s-%s" action phase))
					       (cl-loop for config in n-configs
							append (mk-code/:feature-config config
											name
											action
											phase))))))
      (cl-loop for action in '(pkglist activate)
	       do (setf c1
			(plist-put c1
				   (intern (format ":%s-ignore" action))
				   (cl-loop for config in n-configs
					    append (mk-code/:feature-config config
									    name
									    action
									    'ignore)))))
      (DEBUG! "Scope/init scope = \n%s c1 =\n%s"
	      (Scope/to-string scope)
	      (pp-to-string c1))
      (setf code c1))))

(defclass Core-Scope (Feature-Scope)
  ()
  "Core scope for easy-emacs")

(defclass Editor-Scope (Feature-Scope)
  ()
  "Editor scope for easy-emacs")

(defclass UI-Scope (Feature-Scope)
  ()
  "Editor scope for easy-emacs")

(defun merge-keybinds (c1 c2)
  (let ((keybinds (plist-get c1 :keybinds))
	(keybinds2 (plist-get c2 :keybinds)))
    (setq key (pop keybinds2))
    (while keybinds2
      (setq def (pop keybinds2))
      (unless (plist-get keybinds key)
	(setq keybinds
	      (plist-put keybinds
                         key def)))
      (setq key (pop keybinds2)))
    keybinds))

(defun merge-features (c1 c2)
  (let ((features (plist-get c1 :features))
	(features2 (plist-get c2 :features)))
    (cl-loop for feature in features2
             do (when (not (has-feature? features feature))
                  (setq features
			(append features (list feature)))))
    features))

(defun do-merge-mode-configs (c1 c2)
  (let ((keybinds (merge-keybinds c1 c2))
	(features (merge-features c1 c2)))
    (plist-put
     (plist-put c1
		:keybinds keybinds)
     :features
     features)))

(defun do-inherit-mode-config (config inherit data)
  (if inherit
      (let ((parent-config (plist-get data inherit)))
	(if parent-config
	    (do-inherit-mode-config (do-merge-mode-configs config parent-config)
				    (plist-get parent-config :inherit)
				    data)))
    config))

(defun inherit-mode-config (config data)
  (if (plist-get config :attach)
      (do-inherit-mode-config config
			      (plist-get config :inherit)
			      data)
    nil))

(defun merge-mode-configs (configs)
  (let ((config-data nil))
    (cl-loop for config in configs
	     do (setq config-data
		      (plist-put config-data
				 (plist-get config :name)
				 config)))
    (DEBUG! "merge-mode-configs configs =\n%s config-data = \n%s"
	    (pp-to-string configs)
	    (pp-to-string config-data))
    (let ((h-configs (cl-remove nil
				(cl-loop for config in configs
					 collect (inherit-mode-config config config-data)))))
      (DEBUG! "merge-mode-configs h-configs =\n%s"
	      (pp-to-string h-configs))
      h-configs)))

(defclass Mode-Scope (Normalized-Scope)
  ()
  "Mode scope for easy-emacs")

(defun make-mode-feature-call (feature action phase options)
  (DEBUG! "make-mode-feature-call feature %s action %s phase %s options %s"
	  feature action phase options)
  (let ((fn (car feature))
	(status (cadr feature)))
    (pcase action
      ('activate `(;; FIXME: Following can NOT work correctly
		   ;; (setq z-options ',(plist-put options :status status))
		   (setq z-options
			 (plist-put ',options
				    :status
				    ,status))
		   (invoke-feature ',fn 'activate 'modes 'ignore
				   z-options)))
      ('pkglist `((progn
		    (setq z-options
			  (plist-put ',options
				     :status
				     ,status))
		    (invoke-feature ',fn 'pkglist 'modes 'ignore
				    z-options))))
      (_ `((setq z-options
		 (plist-put ',options
			    :status
			    ,status))
	   (invoke-feature ',fn ',action 'modes ',phase
			   z-options))))))

(defun mk-code/:mode-config (config action phase)
  (let ((options (plist-get config :options))
	(cn (plist-get config :name))
	(features (plist-get config :features)))
    (when features
      (setf options
	    (plist-put options :mode cn))
      (cl-loop for feature in features
	       do (setf options
			(plist-put options
				   (mk-keyword (car feature))
				   (cadr feature))))
      (let ((code nil))
	(cl-loop for feature in features
		 do (setf code
			  (append code
				  (let ((z-options options))
				    (make-mode-feature-call feature action phase z-options)))))
	(DEBUG! "mk-code/:mode-config code =\n%s"
		(pp-to-string code))
	(if (eq action 'activate)
	    (let ((c1 (cl-loop for mode in (plist-get config :attach)
			       collect `(add-hook ',(intern (format "%s-hook" mode))
						  ',(intern (format "call-%s/:activate" cn)))))
		  (c2 (cl-remove nil
				 (cl-loop for mode in (plist-get config :attach)
					  append `(,(mk-mode-keybinds mode
								      (plist-get config :keybinds)))))))
	      `((progn
		  (defun ,(intern (format "call-%s/:activate" cn)) ()
		    ,@code)
		  ,@(append c1 c2))))
	  code)))))

(cl-defmethod Scope/init ((scope Mode-Scope))
  (with-slots (configs n-configs code) scope
    (setf n-configs
	  (merge-mode-configs
	   (cl-loop for config in configs
		    collect (normalize-mode-config config))))
    
    (let ((c1 nil))
      (cl-loop for action in '(prepare configure)
	       do (cl-loop for phase in '(before primary after)
			   do (setf c1
				    (plist-put c1
					       (intern (format ":%s-%s" action phase))
					       (cl-loop for config in n-configs
							append (mk-code/:mode-config config
										     action
										     phase))))))

      (cl-loop for action in '(pkglist activate)
	       do (setf c1
			(plist-put c1
				   (intern (format ":%s-ignore" action))
				   (cl-loop for config in n-configs
					    append (mk-code/:mode-config config
									 action
									 'ignore)))))
      (setf code c1)
      (DEBUG! "Scope/init code = \n%s"
	      (pp-to-string code)))))


(defclass Completion-Scope (Feature-Scope)
  ()
  "Completion scope for easy-emacs")

(defclass App-Scope (Feature-Scope)
  ()
  "App scope for easy-emacs")

(defun mk-scope (type configs)
  (DEBUG! "mk-scope type = %s\n configs = \n%s"
	  type (pp-to-string configs))
  (pcase type
    (:init (make-instance 'Initial-Scope
			  :name 'init
			  :configs configs))
    (:core (make-instance 'Core-Scope
			  :name 'core
			  :configs configs))
    (:editor (make-instance 'Editor-Scope
			    :name 'editor
			    :configs configs))
    (:ui (make-instance 'UI-Scope
			:name 'ui
			:configs configs))
    (:modes (make-instance 'Mode-Scope
			   :name 'modes
			   :configs configs))
    (:completion (make-instance 'Completion-Scope
				:name 'completion
				:configs configs))
    (:app (make-instance 'App-Scope
			 :name 'app
			 :configs configs))
    (_ (make-instance 'Scope
		      :name type
		      :configs configs))))

(defun mk-scopes (args)
  (cl-loop for type in '(:init :core :editor :ui :modes :completion :app)
	   collect (mk-scope type (plist-get args type))))

(provide 'core-scope)
