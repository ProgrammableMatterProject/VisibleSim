# VisibleSim Technical Reference

This technical reference sheet should be used as a starting point for anyone who is interested in contributing to VisibleSim. It details how the repository is organised, attempts to clearly outline the software's architecture, and provides new contributors with everything they need to know about the coding style to adopt.

## Repository Organisation
### Project Tree
The VisibleSim repository is organised as-follows:
```
.
+-- applicationsBin/
|   +-- [BlockCode Applications]
|	+-- meld/
+-- applicationsSrc/
|	+-- Makefile
|	+-- [BlockCode Sources]
+-- doc/
|	+-- Doxyfile
|	+-- html/
|	+-- Makefile
|	+-- [Various Documentations]
+-- simulatorCore/
|	+-- src/
|	|	+-- TinyXML/
|	|	+-- Debugger/
|	|	+-- [Core Source Files]
| 	+-- [Texture Directories]
|	+-- [UI Help Files]
+-- utilities/
|	+-- meld-compiler/
|	+-- LMParser/
|	+-- configGenerator/ (nazandre/VisibleSimConfigGenerator)
|	+-- blockCodeTest.sh
```
Here is a quick look at the content of each of the main directories from the project tree, before diving into details:

- simulatorCore: simulator C++ sources and graphical textures
- applicationsSrc: user programs (named block codes) source files  
- applicationsBin: user programs executables and configuration files 
- docs: all sort of documentations
- utilities: contains scripts and programs for compiling and testing, as well as useful tools for users

### Source Files
As mentioned above, the C++ source files for the simulator's core are all stored into the `simulatorCore/src` directory. The `.ccp` file extension is used for implementation files, and `.h` for header files.

### TODO

## Compilation Workflow

### TODO

## Doxygen Documentation
The source code and API are documented using [Doxygen](http://www.stack.nl/~dimitri/doxygen/).
### Viewing 
First, if the `doc` folder does not exist yet, simply run `make doc` from the root directory to build it.  Then, you can `doc/html/index.html` in your favorite browser. 
### Contributing
For now, please refer to the [Official Doxygen Documentation](http://www.stack.nl/~dimitri/doxygen/manual/docblocks.html) for advices on how to document the source code.

## Coding Style
The following rules should be taken into account when contributing new source code to VisibleSim, as they are the coding practises used on existing code, and homogeneous coding style makes it a lot easier to understand the project's sources.

### Whitespace and Indentation
Indentation size is at 4 spaces per logic level. No tabs. No trailing whitespace at the end of a line. Newline characters should be UNIX style (use `\n`, not `\r\n`).
__Warning__: There is no additional indentation inside a _Namespace_ logical unit.

### Line Length
Lines of codes (LoC) should be kept as short as possible to avoid formatting issues while diff-ing, using split-views, or printing. Ideally, lines of codes should not be longer than __80__ characters, nonetheless, you may sometimes see LoC of up to __100__ characters in VisibleSim. It is highly discouraged to exceed this limit, since it makes code a lot harder to read.
Hence, please break long conditions after && and || logical operators, and fold long function parameter lists on several lines.

### Bracing Style 
Use [K&R bracing style](https://en.wikipedia.org/wiki/Indent_style#K.26R_style): opening brace at end of first line, cuddle else on both sides.

### General C++ Style Guide
Insightful guidelines for contributing to C++ source code can be found [here](https://google.github.io/styleguide/cppguide.html).

## ...