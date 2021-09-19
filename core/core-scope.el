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

(cl-defmethod Scope/Pkgs:update ((scope Scope))
  t)

(cl-defmethod Scope/Pkgs:get ((scope Scope))
  nil)

(cl-defmethod Scope/mk-code ((scope Scope) action phase)
  (let ((key (intern (format ":%s-%s" action phase))))
    (with-slots (code) scope
      (DEBUG! "Scope/mk-code action %s phase %s \n scope =\n%s \ncode=\n%s"
	      action phase
	      (pp-to-string scope)
	      (pp-to-string code))
      (let ((c (plist-get code key)))
	(DEBUG! "Scope/mk-code () =\n%s"
		(pp-to-string c))
	c))))

(defclass Initial-Scope (Scope)
  ()
  "Scope for initialization")

(cl-defmethod Scope/init ((scope Initial-Scope))
  (with-slots (configs code) scope
    (setf code
	  (plist-put nil
		     :prepare-before
		     `,@configs))))

(defclass Feature-Scope (Scope)
  ((n-configs :initform nil))
  "Scope with list of features")

(defun normalize-feature-config (config)
  (let ((fn (car config))
	(options (normalize-options (cdr config)))
	(n-config nil))
    (setq n-config
	  (plist-put 
	   (plist-put options :name fn)
	   :status 1))
    n-config))

(defun normalize-mode-config (config)
  (let ((cn (car config))
	(options (normalize-options (cdr config)))
	(n-config nil))
    (setq n-config
	  (plist-put options
		     :name cn))
    ;; single inherit
    (let ((inherit (plist-get n-config :inherit)))
      (when inherit
	(setq n-config
	      (plist-put n-config
			 :inherit (car inherit)))))
    ;; pare features
    (let ((features (plist-get n-config :features)))
      (when features
	(setq n-config
	      (plist-put n-config
			 :features
			 (parse-features features)))))
    n-config))

(defun mk-code/:feature-config (config scope action phase)
  (let ((options config)
	(fn (plist-get config :name)))
    (pcase action
      ('activate `(,@(plist-get config :before-activate)
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
      (setf c1
	    (plist-put c1
		       :activate-ignore
		       (cl-loop for config in n-configs
				append (mk-code/:feature-config config
								name
								'activate
								'ignore))))
      (DEBUG! "Scope/init scope = \n%s c1 =\n%s"
	      (pp-to-string scope)
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
    
(defclass Mode-Scope (Scope)
  ((n-configs :initform nil))
  "Mode scope for easy-emacs")

(defun make-mode-feature-call (feature action phase options)
  (DEBUG! "make-mode-feature-call feature %s action %s phase %s options %s"
	  feature action phase options)
  (let ((fn (car feature))
	(status (cadr feature)))
    (pcase action
      ('activate `((setq z-options
			 (plist-put ',options
				    :status
				    ,status))
		   (invoke-feature ',fn 'activate 'modes 'ignore
				    z-options)))
      (_ `((setq z-options
		 (plist-put ',options
			    :status
			    ,status))
	   (invoke-feature ',fn ',action 'modes ',phase
			    z-options))))))

(defun mk-code/:mode-config (config action phase)
  (let ((options nil)
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
      (setf c1
	    (plist-put c1
		       :activate-ignore
		       (cl-loop for config in n-configs
				append (mk-code/:mode-config config
							     'activate
							     'ignore))))
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
