# VisibleSim Technical Reference

This technical reference sheet should be used as a starting point for anyone who is interested in contributing to VisibleSim. It details how the repository is organised, attempts to clearly outline the software's architecture, and provides new contributors with everything they need to know about the coding style to adopt.

## Repository Organisation
### Project Tree
The VisibleSim repository is organised as-follows:
```shell
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

- `simulatorCore`: simulator C++ sources and graphical textures
- `applicationsSrc`: user programs (named block codes) source files  
- `applicationsBin`: user programs executables and configuration files 
- `docs`: all sort of documentations
- `utilities`: contains scripts and programs for compiling and testing, as well as useful tools for users

### Source Files
As mentioned above, the C++ source files for the simulator's core are all stored into the `simulatorCore/src` directory. The `.cpp` file extension is used for implementation files, and `.h` for header files.

### Software Architecture 
TODO
## Compilation Workflow
### Overview
The compilation process in VisibleSim is based on a recursive approach. The root folder contains the root folder Makefile, and from here the `make` command is passed to Makefiles from sub-directories, along with additional information (flags, variables). 
### Root Makefile
The top level Makefile is the one that should receive the original `make` command. It contains a few conditional variable assignments, whose need is justified as-follows:
 
- __OS__: We need distinct variables for distinct OS families, because both the manner of including libraries, and their implementations themselves are different hence the potential need for custom compilation and linking flags. Also, even though we recommend using the gcc compiler, OS X users are more likely to be using Clang, which also has its special set of flags.
- __MELD PROCESS__: Recently, the source code has been updated to use the new features of the C++11 standard. In an effort for better portability, we have replaced the features brought by the `boost` library by their `std` counterpart present in C++11. This has been possible for all features except `asio`, used for interprocess communication with the Meld Virtual Machines. Since it will eventually be entirely replaced by the Meld Interpreter, we decided to exclude the Meld Process sources from the compilation by default (and thus, `boost`). If you want to enable it nonetheless, add the `-DENABLE_MELDPROCESS` flag to the `TEMP_CCFLAGS` list.

The root makefile can be used to propagate the `test` and `doc` special directives, that will be detailed later. By default, it only compiles the sources from `simulatorCore/src` and all the block codes marked for compilation in the `applicationsSrc` Makefile.

### Core Compilation
The Makefile in `simulatorCore/src` handles the compilation of the core of VisibleSim, and its output is the following:

- `lib/`, contains libraries (one per module family), to be used by the user for compiling block codes independently from the simulator's core
- `deps/`, contains dependency files for efficiently updating the target if the sources changes
- `obj/`, contains all the output `.o` files

The compilation workflow is the following, in order:

1. Create output directories
2. Dependencies are generated (thanks to `-MT -MMD -MP -MF` CC flags)
2. Module-independent source files are compiled
3. For each respective module family its specific files are compiled and its library is archived

### Block Code Compilation
Once the core has been compiled, the root Makefile calls the `applicationsSrc` Makefile, whose role is to generate the executables for every block codes that is part of the __SUBDIRS__ variable list. 
All it does is to call `make` on the Makefile in each of the subdirectories marked in __SUBDIRS__. 

Alternatively, the user can call `make` directly from the block code directory, if no change has been made to core, since the static libraries are used for linking. 

All the block code Makefiles are identical, except for the following variables that have to be set by the user: `SRCS`, `OUT`, `MODULELIB`. A custom `make test` procedure can also be specified, more on this [in the dedicated section](#autotest).

### VisibleSim Execution Workflow
TODO

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

### Lines Length
Lines of codes (LoC) should be kept as short as possible to avoid formatting issues while diff-ing, using split-views, or printing. Ideally, lines of codes should not be longer than __80__ characters, nonetheless, you may sometimes see LoC of up to __100__ characters in VisibleSim. It is highly discouraged to exceed this limit, since it makes code a lot harder to read.
Hence, please break long conditions after && and || logical operators, and fold long function parameter lists on several lines.

### Bracing Style 
Use [K&R bracing style](https://en.wikipedia.org/wiki/Indent_style#K.26R_style): opening brace at end of first line, cuddle else on both sides.

### General C++ Style Guide
Insightful guidelines for contributing to C++ source code can be found [here](https://google.github.io/styleguide/cppguide.html).

## Command Line Interface
### Usage
```shell
> ./<app> [-c <conf.xml>] [-p <meldProgram.bb> -k <module>] ...
VisibleSim options:
	 -f 		full screen
	 -p <name>	program file (Meld for instance)
	 -D 		debugging mode (used in Meld only)
	 -c <name>	xml configuration file
	 -r 		run realtime mode on startup
	 -R 		run fastest mode on startup
	 -x 		terminate simulation when scheduler ends 
	            (Graphical Mode only)
	 -t 		terminal mode only (no graphical output)
	 -s [<maximumDate> | inf] 	
			    Scheduler mode:	Default, stops when event list is empty
				maxDate (ms): the scheduler will stop when even list is 
				              empty, or when the maximum date is reached
				inf: the scheduler will have an infinite duration 
					 and can only be stopped by the user
	 -m <VMpath>:<VMport>	path to the MeldVM directory and port
	 -k {"BB", "RB", "SB", "C2D", "C3D"} module type for meld execution
	 -g 		Enable regression testing
	 -h 	    help
```

### Options

##### Full Screen (`-f`)
Start the VisibleSim graphical window in full screen mode.
##### Specifying Meld Program (`-p <meldProg.bb>`)
When using a `meld` application, tells the simulator what Meld program should be executed. `./program.bb` is its default value.
##### Debugging Mode (`-D`)
Enable debugger, for step-by-step execution, and interrogating variables. Only available inside `meld` (Process) applications for now. 
##### Specifying Configuration File (`-c <config.xml>`)
Tells VisibleSim what XML configuration file should be loaded into the simulation. `./config.xml` by default. Please refer to the appropriate section for details on formatting the configuration file.
##### Immediate Simulation Start (`[-r | -R]`)
Enable immediate start of the scheduler when the simulation is started. 

- `-r`: Starts the simulation in `realtime` mode (Slower event processing for viewing)
- `-R`: Starts the simulation in `fastest` mode (Process events as fast as possible)

If none of these two options is provided on the command line, the user will have to manually press the <kbd>r</kbd> or <kbd>R</kbd> keys to start the simulation.
##### Simulator Autostop (`-x`)
Terminates the simulation (_i.e. closes VisibleSim_) when all events have been processed by the scheduler.
##### Terminal mode (`-t`)
Runs the simulation without the graphical OpenGL window. It also implicitly includes the `-R` and `-x` options, since the simulation will start right away and stop on scheduler end.
##### Scheduler Termination Mode (`-s [<maximumDate> | inf]`)
Configures the conditions for the simulation to end:

- __Default__ (no option specified): simulation will stop when all events have been processed by the scheduler _OR_ when <kbd>q</kbd> is pressed from the simulation window. 
- __Bounded__ (`-s maxDate`): similar to __default__, but also stop simulation if `maxDate` has been reached. `maxDate` is expressed in milliseconds.
- __Infinite__ (`-s inf`): simulation continues even though all events have been processed. For now, the scheduler will still stop if the date reaches `UINT64_MAX`, and of course, if the graphical simulation window is closed by the user.

##### Meld Process I/O Setup (`-m <VMpath>:<VMport>`)
Only used when running a program in `Meld Process` mode, to specify the location and port of the Meld Process VM, for communicating with VisibleSim.
##### Specify Modular Meld Target Module  (`-k {"BB", "RB", "SB", "C2D", "C3D"}`)
This option is only available if running the `applicationsBin/meld/meld` executable (generic `meld` executable for modular robots), and is used for specifying the target block family.
##### Regression Testing Export (`-g`)
This option triggers the export of the current configuration at the end of the simulation to an XML file named `./confCheck.xml`. This is especially useful for regression testing of the block codes, which is detailed in its own section.
##### Help (`-h`)
Displays the usage message in the terminal.

## Meld
TODO
### Building
Inclusions
### Running
## Block Code API
TODO
## Clock
TODO
## User Interactions
### Generic Menu
### C2D Rotations
### Tap
TODO
## Statistics
TODO
## <a name="autotest"></a>Automated BlockCode Testing
TODO

## Configuration Files
In VisibleSim, a configuration file is a XML file that describes the simulated world. At least the state of the world at the beginning of the simulation has to be described, but thanks to a feature named __targets__, the objective state of the world at certain points in the simulation can also be described. 

### Brief Example 
```xml
<?xml version="1.0" standalone="no" ?>
<world gridSize="10,10,10" windowSize="1024,800">
  <camera target="200,200,200" directionSpherical="0,70,400" angle="45"/>
  <spotlight target="200,20,200" directionSpherical="45,60,500" angle="40"/>
  
  <blockList color="255,255,0">
    <block position="5,5,5"/>
    <block position="5,5,6"/>
    <block position="5,5,7"/>
    <!-- Small Cross: -->
    <block position="4,5,6" color="255,0,0"/>
    <block position="6,5,6" color="255,0,0"/>
    <!-- Big Cross: -->	
    <block position="5,5,8" color="0,0,255"/>
    <block position="5,5,9" color="0,0,255"/>
    <block position="3,5,7" color="0,0,255"/>
    <block position="4,5,7" color="0,0,255"/>
    <block position="6,5,7" color="0,0,255"/>
    <block position="7,5,7" color="0,0,255"/> 
    <!-- Tail: -->
    <block position="5,6,5" color="0,255,255"/>
    <block position="5,7,5" color="0,255,255"/>
    <block position="5,8,5" color="0,255,255"/>
  </blockList>
</world>
```

This configuration file would produce the following output if using BlinkyBlocks :

![Sample BlinkyBlocks Configuration](https://i.imgsafe.org/1700a14b52.png "SampleConfiguration")

### XML Attributes
Every possible configuration file attribute is detailed below, where items marked as "!`item`" are mandatory.
#### !`XML Declaration`
```xml
<?xml version="1.0" standalone="no" ?>
```
This line should be included as the first line in any XML file to identify it as such, hence VisibleSim configuration files are no exception.
#### !`world`
This is the root element of the configuration, and all other elements are linked to it. It is defined as-follows:
```xml
<world gridSize="x,y,z" windowSize="w,h">
	<!-- Configuration: ... -->
	<!-- camera -->
	<!-- spotLight -->
	<!-- blockList -->
	<!-- targetList -->
</world>
```
Attributes:

- !`gridSize="x,y,z"`: Size of the lattice in each coordinate (x, y, z).
- `windowsize="w,h"`: Width and height of the graphical simulation window. _1024x800_ if unspecified, ignored if in _terminal mode_.

#### !`Camera` and !`spotlight`
These elements respectively describe the initial position and orientation of the graphical window's view and lighting. 

```xml
<camera target="xtc,ytc,ztc" directionSpherical="rc,θc,φc" angle="αc"/>
<spotlight target="xts,yts,zts" directionSpherical="rs,θs,φs" angle="αs"/>
```
Attributes:

__TODO:__ Need explanations on what are these parameters, and how they can be found.

- !`target="x,y,z"`: Specifies the location of the camera in the simulated world. 
- !`directionSpherical="r,θ,φ"`: Where (r, θ, φ) are the spherical coordinates of the object's direction.
- !`angle="α"`: __TODO?__.  Based on observations: Field of view?. 

__N.B.:__ NOT necessary if using  _terminal mode_.

#### !`blockList` 

The `blockList` element describes the starting physical position and color of modules (+ extra attributes depending on module type) in the simulated world, as well as their logical identifier for simulation. 
There are two types of children elements that can be used to describe the ensemble, and that can be combined, `block` and `blocksLine`.

```xml
<blockList color="r,g,b" ids="[MANUAL|ORDERED|RANDOM]" step="sp" seed="sd">
	<!-- Description of all blocks in simulation -->
	<block position="x,y,z" color="r,g,b" master="true/false" id="i"/>;
	<!-- ... -->
	<blocksLine plane="p" line="l" color="r,g,b" values="00101...1110"/>
	<!-- ... -->
</blockList>
```
Attributes: 

- `color="r,g,b"`: The default color of the modules. If unspecified, set to dark grey.
- `ids"`: Please refer to [Module Identifiers Assignment Schemes](#ids) below.

##### <a name="ids"></a>Module Identifiers Assignment Schemes
In VisibleSim, every module is given a unique numerical identifier (from `1` to `2^63`), to which we will refer as `blockID`, or simply `ID`. They are variables that can be used in algorithms, and if they are always identical, it can introduce determinism, hence we provide the user with several methods for assigning each module with an `ID`. 

```xml
<blockList color="r,g,b" ids="ORDERED">
	<block position="x1,y1,z1"/>
	<!-- ... -->
	<block position="xN,yN,zN"/>
</blockList>
<!-- OR -->
<blockList color="r,g,b" ids="MANUAL">
	<block position="x45,y45,z45" id="45"/>
	<!-- ... -->
	<block position="x3,y3,z3" id="3"/>
</blockList>
<!-- OR -->
<blockList color="r,g,b" ids="RANDOM" step="sp" seed="sd">
	<block position="x?,y?,z?"/>
	<!-- ... -->
	<block position="x?,y?,z?"/>
</blockList>
```

The `ids` attribute of `blockList` is used for specifying the scheme that should be used. Let `N` be the number of modules in the configuration. There are 3 possible values for the `ids` field :

1. `ORDERED`: Assign `ID` to modules based on their order of appearance in the configuration file. Starting from `1`, hence last module will be identified as `N`.
2. `MANUAL`: Manually assign an `ID` to each module using the `id` attribute of `block`. Every single module in the configuration file has to be manually assigned an identifier.
3. `RANDOM`: Takes two optional `blockList` parameters, `seed` and `step`. Fill a container with `N` identifiers starting from `1` and distanced to each other by `step`. Then, randomly assign each identifiers to a module, with the possibility to provide a custom `seed` for a deterministic distribution. If `step` equals `1` if unspecified, and if `seed` is unspecified, a random one is used. 

__N.B.:__ If `ids` is unspecified, `ORDERED` will be used by default. 

__N.B.2:__ It is the responsibility of the user to ensure that the parameters used in `RANDOM` mode (_i.e._ number of modules and step), will not generate an overflow (`ID` > `1 + (n - 1) * step`). The behaviour of VisibleSim is undefined in that case.

__N.B.3:__ An absence of block is marked as an `ID` of `0`, but this is beyond the scope of configuration files.

##### The `block` Element
Defines a single module and its attributes. 
```xml
<block position="x,y,z" color="r,g,b" master="true/false" id="i"/>;
```
Attributes:

- !`position="x,y,z"`: The position of the module on the lattice. Has to be unique.
- `color="r,g,b"`: The color of the module. Set to the `blockList` color attribute if undefined.
- `master="true/false"`: Indicates if a module has the role of __master__. _false_ if undefined.
- `id="i"`: Unique identifier of the module. Only used if module assignment scheme is set to manual. 

##### The `blocksLine` Element
Defines an entire line of modules, but with less control over each module's attributes. 
```xml
<blocksLine plane="p" line="l" color="r,g,b" values="ab001...01z"/>
```
Attributes:

- !`line="l"`: The line on which the modules should be placed. (_i.e._ the `y` coordinate, `0` if unspecified)
- !`plane="p"`: The plane on which the modules should be placed. (_i.e._ the `z` coordinate, `0` if unspecified)
- `color="r,g,b"`: The color of the modules. Set to the `blockList` color attribute if undefined.
- !`values="ab001...01z"`: Describes the line of modules, with `W` (width of the lattice) binary values, where `0` means an absence of block, and `1` means presence of block. Here, `a` means the cell at position `(0, l, p)` of the lattice, `b`: `(1, l, p)` ... and `z`: `(W - 1, l, p)`.

__Warning:__ if the number of values is not equal to the width of the lattice, the configuration is incorrect. 
__Warning#2:__ `blocksLine` can only be used with the `ORDERED` identifier scheme.
 
##### Constraints
Both description methods are subject to a number of constraints:

- Two modules cannot be placed on the same lattice cell. (i.e. No `block` or `blocksLine` elements should overlap)
- Every described module needs to have a position. 
- Two modules cannot have the same ID. 

#### Reconfiguration Targets
As mentioned earlier, a VisibleSim configuration can also be used to describe one or multiple reconfiguration `targets` (_i.e._ an objective configuration in term of module positions and colors). 

##### Description
Description of `targets` in the XML configuration file has to be done under the `targetList` children element of `world`. It defines a list of configurations that will be read one by one in sequential order, when required in the user block code program.

```xml
<targetList>
  <target format="grid">
    <cell position="x1,y1,z1"/>
    <!-- ... -->
    <cell position="xM,yM,zM" color="rM,gM,bM"/>    
  </target>
  <target format="csg">
    <!-- NOT YET IMPLEMENTED -->
  </target>
  <!-- other targets -->
</targetList>
```

For each `target` children element of `targetList`, its `format` attribute indicates the model used to describe the target:

- `format="grid"`: describes a target configuration in term of individual cells.
- `format="msg"` : describes a target configuration in term of a combination of geometrical shapes.

###### Grid Target
A `cell` element is used to declare that a specific cell of the lattice is part of the target, and if necessary, what color the module on this cell should have. 
```xml
<cell position="x,y,z" color="r,g,b"/>    
```
Attributes: 
- !`position="x,y,z"`: The position of the cell to add to the `target` definition. 
- `color="r,g,b"`: The target color of the module on this cell. If unspecified, the color will be initialised as `(0,0,0)`, but should be not be used.
###### CSG Target
`NOT YET IMPLEMENTED`

##### API: Using Targets in Block Codes

```C++
//	Target.h
    /**
     * @brief Parse next target from the configuration file's 
     * Target List, and return a pointer to the instantiated object
     * @return pointer to the parsed Target object, or NULL if there are 
     * no (more) targets in the configuration file
     */
    static Target *loadNextTarget();    
    /**
     * @brief Indicates if a position belongs to the target
     * @param pos position to consider
     * @return true if pos belongs to the target, false otherwise
     */
    bool isInTarget(const Cell3DPosition &pos) = 0;
    /**
     * @brief Returns the target color at position pos
     * @param pos position to condiser
     * @return target color at cell p
     */
	const Color getTargetColor(const Cell3DPosition &pos);
	//!< Prints a target to an output stream
    friend ostream& operator<<(ostream& out,const Target &t);

// BlockCode.h
    /**
     * @brief Loads the next target from the configuration file 
     * into the target attribute by calling Target::loadNextTarget()
     * @return true if a target has been loaded, 
     *         false otherwise (No target remaining in config file)
     */
    static bool loadNextTarget();
```

The `targets` API is quite straighforward:

- The `BlockCode` abstract class contains a `static` pointer to a `target`, hence any user block code will also have access to that variable. It can be accesses using `<blockCodeName>::target`.
- Upon VisibleSim startup, `<blockCodeName>::target` will be loaded with the first occurence of `target` from the configuration file. If none is defined, then `<blockCodeName>::target` will be `NULL`.
- Once a `target` is loaded, the following functions can be used: 
	- `isInTarget`: check whether or not a given cell is part of the target. 
	- `getTargetColor`: get the the target color for a given cell. If not part of the target, `InvalidPositionException` is raised.
	- `<ostream> << <blockCodeName>::target`: prints the target to an output stream.
- Finally, use `<blockCodeName>::loadNextTarget()` to read the next target from the configuration file, and store it into `<blockCodeName>::target`. If there was no additional target defined, this function will return `false`, and `target` attribute will be `NULL`.

#### Parsing Additional User Elements
Users can add extra elements to the configuration file that are specific to their application. The VisibleSim API provides a function to the users that is called only once per simulation, right after the parsing of the _core_ elements of the configuration file. 
```C++
/**
 * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from it. Has to be overriden in the child class.
 * @param config : pointer to the TiXmlDocument representing the configuration file, all information related to VisibleSim's core have already been parsed
 *
 * Called from BuildingBlock constructor, only once.
 */ 
 virtual void parseUserElements(TiXmlDocument *config) { }
```

This function defined in `blockCode.h` is `virtual` and does nothing by default, but can be overloaded in the user `BlockCode` to perform additional configuration. It can be used in the three following ways:

1. Do not overload `parseUserElements`. Performs no additional parsing.
2. Overload `parseUserElements` an use the `TiXmlDocument *config` parameter to access and parse the user elements from the configuration file using [TinyXML](http://www.grinninglizard.com/tinyxml/index.html). (Please refer to the [Official TinyXML Documentation](http://www.grinninglizard.com/tinyxmldocs/index.html) for instructions)
3. Overload `parseUserElements`, but ignore the XML document parameter, and use this function to perform custom data reading and parsing from another file, in any way.