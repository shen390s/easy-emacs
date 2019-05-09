# README for Microsoft Python Language Server

## Installation

1. Install [dotnet-sdk](https://www.microsoft.com/net/download)

2. Clone and install [python-language-server](https://github.com/Microsoft/python-language-server)
 
	```
		git clone https://github.com/Microsoft/python-language-server.git
		cd python-language-server/src/LanguageServer/Impl
		dotnet build -c Release
	```
	
	If you choose, compile the language server to a single executable with:
	```
		dotnet publish -c Release -r osx-x64
	```
		
	Change the value of the -r flag depending on your architecture and operating system. See 
	Microsoft’s Runtime ID Catalog for the right value for your system.

3. Then, link the executable to somewhere on your path, e.g.
	
	```
		ln -s $(git rev-parse --show-toplevel/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer ~/.local/bin/
	```
