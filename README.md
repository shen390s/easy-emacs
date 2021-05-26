# Easy-emacs

## Quick start

  1. clone this repository as .emacs.d in your home directory.
  
  2. edit .emacs.d/easy-emacs-config.el to meet your requirement
  
## Feature highlights

  1. Easy for configuration
     
	 forget mode hooks
	 
  2. Fast startup
  
  3. Language support
	 - Support languages
		- C/C++, python, rust, java,R,plantuml, graphviz,clojure
		
	 - Language server support
		- C/C++. cquery and ccls
		- python. Microsoft python language server
		- rust. rls
		- Java language server
		- Both support LSP and eglot
	 
  4. Live preview for markdown
  
  5. Ivy complete
  
  6. Other misc feature 
	 - powerline/smartline
	 - easy color theme support
	 - keybinding (evil, boon ergoemacs)
     - rainbow delimiters
	 - rainbow identifiers
	 - smartparens
	 - emacs server
	 
  7. for china user
     - clone https://github.com/shen390s/git_best_mirrors.git
     - set GIT_BEST_MIRROR_TOOL_DIR to the bin sub-directory of cloned 
       directory to enable using fast github mirrors from china

## Hack Easy-Emacs

### Basic conception

	- scope
	
	  
	- feature
	
	  Easy-Emacs uses feature to customized the behavior of system, such
	  as feature cquery will enable using cquery as C/C++ language server 
	  and ccls will enable using ccls as C/C++ language server.
	  
	- package
	
	  Package defines how Easy-Emacs install packages, currently Easy-Emacs
	  support install package from ELPA, MELPA source and github repository
	  
### How to?

#### define a new package

Package is defined using command *package-origin!*:

```{elisp}
(package! my-package
			 "The description of my package"
			 (my-package :type git
						 :host github
						 :repo "my-github-id/my-package"))
```

#### define a feature

The feature is defined using command *feature!*:

```{elisp}
(defun my-feature-pre-config-function ()
  ;; code goes here
  ;; everything is ok, this function will return t
  t)

(defun my-feature-activate-function (&rest args)
  ;; real code goes here
  t)

(defun my-feature-deactivate-function ()
  ;; real code goes here
  t)

(feature! my-feature
		  "The description of my feature"
		  ( a list of packages which the feature depends on)
		  my-feature-config-function
		  my-feature-prepare-function
		  my-feature-activate-function)
```

#### Other

You can check Easy-Emacs source code in core directory and all features
supported by Easy-Emacs in modules directory.

## Troubleshooting

If you find any problem, you can:
- disable byte compile and remove all elc files for easy to troubleshooting.
- using command *log!* to trace the running of code

## Contribute

- fork this repository
- fix bugs or add more features
- send pull requests
