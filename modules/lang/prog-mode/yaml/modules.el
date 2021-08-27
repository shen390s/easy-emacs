;; -*- lexical-binding: t -*-

(package! yaml-mode
	     "A major Emacs mode for edit yaml document"
	     (yaml-mode :type git
			:host github
			:repo "yaml/yaml-mode"))

(autoload-r! yaml-mode
	     (yaml-mode)
	     "yaml-mode"
	     t)

