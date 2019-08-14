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
	 
## Hack Easy-Emacs

### Basic conception

	- scope
	
	  Easy-Emacs uses scope to control the on/off of features. A scope
	  can have parent scope which it can inherit features from parent
	  scope
	  
	- feature
	
	  Easy-Emacs uses feature to customized the behavior of system, such
	  as feature cquery will enable using cquery as C/C++ language server 
	  and ccls will enable using ccls as C/C++ language server.
	  
	- package
	
	  Package defines how Easy-Emacs install packages, currently Easy-Emacs
	  support install package from ELPA, MELPA source and github repository
	  
	- mode
	
	  This is the same as mode in Emacs. A mode in Easy-Emacs will be assigned
	  with a scope to get a list of features.
	  
## Troubleshooting

## Contribute

  - fork this repository
  - fix bugs or add more features
  - send pull requests
