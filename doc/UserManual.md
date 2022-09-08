# VisibleSim User Manual

This manual is intended to be used by new users of VisibleSim, to get them started writing distributed applications for modular robots. 

## Doxygen Documentation
The source code and API are documented using [Doxygen](http://www.stack.nl/~dimitri/doxygen/).
### Viewing 
First, if the `doc` folder does not exist yet, simply run `make doc` from the root directory to build it.  Then, you can `doc/html/index.html` in your favorite browser. 
### Contributing
For now, please refer to the [Official Doxygen Documentation](http://www.stack.nl/~dimitri/doxygen/manual/docblocks.html) for advices on how to document the source code.

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
This section explains aims at detailing the lifecycle of a VisibleSim simulation, for users and contributors to better understand the process.

1. __Entry Point__: `main` function of the BlockCode. 
2.  __Simulator Creation__: `<ModuleNamespace>::createSimulator()` is called. 
	- `ParseConfiguration()`: [Configuration file parsing](#config) and instantiation of all the core componnents of VisibleSim. In order, the following components are instantiated:
		- `Simulator`
		- `World`
		- `Scheduler`
		- `Camera` and `Spotlight` _(ignored in terminal mode)_
		- `BlockList` (__includes individual `BlockCodes`, where `CodeStartEvent` is scheduled at scheduler time `0`__)
		- `Obstacles` _(MultiRobots only)_
		- [`Target`](#target)
	- `ParseUserElements():` While calling the constructor of the first `BlockCode` from the ensemble, the `<BlockCode>::ParseUserElements()` function will be called. [More information here](#UserParsing).
	- `startSimulation()`: Effectively start the simulation by starting the scheduler, or waiting for an input from the user to provide an input to run it.
		- Initialises the neighbourhood information and connects all modules.
		- Starts scheduler if _autostart_ or _terminal mode_ is __ON__.
		- Enter _GLUT_ main loop and start drawing.
3. __Scheduler Run__: Scheduler has been started (either in `fastest` or `realtime` mode)
	- Every module from the ensemble has its `CodeStartEvent` processed, which called its `startup()` function. User algorithm is started. 
	- Running GlutMainLoop until scheduler end. (`waitForSchedulerEnd()`) 
	- Scheduler end occurs when one of the end conditions is met. 
		- __All events processed__.
		- Maximum simulation date has been reached.
		- User explicitly stopped scheduler from the graphical window.
4. __End of Simulation__: Call to `deleteSimulator()` from the main. Cascade deletion of VisibleSim components, in order:
 	- `Scheduler`
	- `Graphical Context`
	- `Simulator`
	- `World` 
		- Every instance of `BuildingBlock`
			- Individual instances of `BlockCode` (_+  Single `Target` static instance_)
			- Individual instances of `Clock`
			- Individual instances of `Stats`
			- Individual instances of `P2PNetworkInterface`
		- Every instance of `GlBlock`
		- `Lattice`
		- `Camera` and `Spotlight`
		- Graphical Object Loaders

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
	 -k {BB, RB, SB, C2D, C3D, MR} module type for generic execution
	 -g 		Enable regression testing
	 -l 		Enable printing of log information to file simulation.log
	 -i 		Enable printing more detailed simulation stats
	 -a <seed>	Set simulation seed
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
##### Specify Modular Meld Target Module  (`-k {BB, RB, SB, C2D, C3D, MR}`)
This option is only available if running a generic `BlockCode` and is used for specifying the target block family, so that the `main` function can deduce what type of `Simulator` to instantiate.
##### Regression Testing Export (`-g`)
This option triggers the export of the current configuration at the end of the simulation to an XML file named `./confCheck.xml`. This is especially useful for regression testing of the block codes, which is detailed in its own section.
##### Log File Output (`-l`)
Enables the printing of any log information to file `simulation.log`, by using this code snippet:
```C++
	OUTPUT << args;
```
If `-l` option is not found, nothing will be printed to the file.
##### Detailed Simulation Statistics (`-i`)
Prints more detailed statistics at the end of the simulation. It prints the minimum, the mean, the maximum and the standard-deviation values of the number of messages sent/received per module, the maximum message queue size reached and the number of motions per module. Be aware that collecting these statistics requires O(number of modules) memory space.
##### Simulation Seed (`-a`)
The randomness of the simulation (variability in the communication rate, variability in the motion duration (not fully supported yet), clock randomness) depends on the simulation seed. Using the same simulation seed on the same configuration produces the same simulation. By default, the simulation seed is equal to 50. If  `-a < seed < 0 >` is used, a randomly generated seed is set. Providing a negative seed (e.g., `-3`) lets the simulator randomly select a seed by itself.
##### Help (`-h`)
Displays the usage message in the terminal.

## Block Code API
This section is intended to guide new users into writing a `BlockCode`, by detailing the provided API, and explaining how to perform the base operations.

### Creating a New Application
First, create a folder in `applicationsSrc/` with the name of your application (to which we will refer as `appName`).

#### Main File

Then, create the `main` file for your application, named `<appName>.cpp`. The following example can be used as a template :
```C++
/* @file <appName>.cpp
 * @author <author>
 * @date <date>
 * A sample main application file.
 */
 
#include "<targetModule>Simulator.h"
#include "<targetModule>BlockCode.h"
#include "<appName>BlockCode.h"

using namespace <targetModule>;

int main(int argc, char **argv) {	
	/* Start simulation by reading configuration file, 
	 * instantiating necessary components, and starting the scheduler.*/
	createSimulator(argc, argv, <appName>BlockCode::buildNewBlockCode);

	/* createSimulator only returns at scheduler end.
     * Can perform some actions here before ending simulation... */

	deleteSimulator(); // Deletion of allocated memory
	
	return(0);
}
```
Where `<targetModule>` is the name of module family for which you are developing the application (e.g. `"<targetModule>Simulator.h"` can be `blinkyBlocksSimulator.h`)

#### Block Code
The implementation of your distributed application will reside in the files `<appName>BlockCode.h`, and `<appName>BlockCode.cpp`, which respectively define a new subclass of `BlockCode` for your application, and implement it.

##### BlockCode Definition
First, create a new class, preferably named `<AppName>BlockCode`, extending either:

-  `BlockCode`, for a generic application that can be used on any kind of `BuildingBlock`, but that may not allow to fully take advantage of the specificities of each type of module. 
	- The only difficulty is that a different type of `Simulator` has to be instantiated for each module. (See the `meld` `BlockCode` in `applicationsSrc` for an example of how this can be handled, using `CommandLine::readModuleType()` and the `-k` command line option)
- `<targetModule>BlockCode`, if creating an application targeting a specific modular robot type. 

The template below can be used to get you started writing `<appName>BlockCode.h`, as it contains all the functions that have to be implemented, and is intended for a generic application. 
In the case of a non-generic application, the parent class should be adapted, and a `static_cast<<targetModule>Block *>(host)` should be used in `buildNewBlockCode()`.

```C++
#ifndef <appName>BlockCode_H_
#define <appName>BlockCode_H_

#include "blockCode.h"
#include "buildingBlock.h"

class <AppName>BlockCode : public BlockCode {
private:
	// custom attribute
public:
    <AppName>BlockCode(BuildingBlock *host) : BlockCode(host) {};
	~<AppName>BlockCode() {};

	/**
	 * @brief This function is called on startup of the blockCode, 
	 it can be used to perform initial configuration of the host or this instance of the distributed program
	*/ 
	void startup();

	/** @brief Returns a new instance of this BlocKCode. Needed to associate code to module.
	 *  @return pointer to a newly allocated instance of this distributed program, for host assignment */
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return (new <AppName>BlockCode(host));
	};
};

#endif /* <appName>BlockCode_H_ */
```

##### Inter-Module Communication API
The `BlockCode` API for communication provides functions to communicate with individual connected modules, or all of them at once (including a variadic parameter list taking pointers to interfaces to ignore). Two versions of the send functions are provided, one taking a `const char *` first argument, that can be used to print a string to the console for easy tracing, and one without.

Also, a message has to be given a __unique__ integer identifier (referred to as `type` below), which can be associated to a message handler via a function pointer using the `addMessageEventFunc()` function. 

Then, when a message is received by a module, the registered handler will be called. If there is none, an error message will be printed to the log file, and the message ignored.

```C++
    /**
     * @brief Add a new message handler to the block code, for message with message type type
     * @param type ID of the message for which a handler needs to be registered
     * @param eventFunc the message handling function as a std::function */
    void addMessageEventFunc(int type,eventFunc);
    /**
     * @brief Send message to all connected interface interfaces, except those in the variadic parameters ignore list.
     * Sending time randomly drawn as follow: 
     *  tt = now + t0 + (rand * dt), where rand is either {0, 1}
     * @param msg message to be sent
     * @param t0 time of transmission (offset to current time)
     * @param dt delta time between two transmissions
     * @param nexcept number of interfaces to ignore
     * @param ... variadic parameters: pointer to the nexcept interfaces to ignore
     * @return Number of messages effectively sent
     */
    int sendMessageToAllNeighbors(Message *msg,int t0,int dt,
								  int nexcept,...);
    /**
     * @copydoc BlockCode::sendMessageToAllNeighbors
     * Identical to sendMessageToAllNeighbors, but prints msgString to the console when the message is sent
     * @param msgString string of the message to be printed when sent
     */
    int sendMessageToAllNeighbors(const char *msgString,Message *msg,
								  int t0,int dt,int nexcept,...);    
    /**
     * @brief Send message to interface dest at time t0 + [0,1]dt
     * @param msg message to be sent
     * @param dest destination interface. 
     * @param t0 base sending time
     * @param dt potential delay in sending time */
    int sendMessage(Message *msg,P2PNetworkInterface *dest,
				    int t0,int dt);
    /**
     * @copydoc BlockCode::sendMessage
     * @param msgString string to be printed to the console upon sending */
    int sendMessage(const char *msgString,Message *msg,
				    P2PNetworkInterface *dest,int t0,int dt);
```

For more information an examples, you can have a look at the already-existing applications provided in the `applicationsSrc` directory.

#### Makefile

In order to compile the application, we provide a sample _Makefile_ that only requires that the user sets the value of a few variables in the top section. Further modifications can be done by the user to suit its needs,  but in most cases, it will suffice.

Please refer to the sample _Makefile_ below, that can be used as template and contains instructions on how to adapt it for your application.

```makefile
# Get current directory's name
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
APPDIR = ../../applicationsBin/$(current_dir)

#####################################################################
#
# --- Sample User Makefile ---
#
# GLOBAL_LIBS, GLOBAL_INCLUDES and GLOBAL_CFLAGS are set by parent Makefile
# HOWEVER: If calling make from the codeBlock directory (for more convenience to the user),
#	these variables will be empty. Hence we test their value and if undefined,
#	set them to predefined values.
#
# You will find instructions below on how to edit the Makefile to fit your needs.
#
# SRCS contains all the sources of your codeBlocks
SRCS = <appName>.cpp <appName>BlockCode.cpp #...
#
# OUT is the output binary, where APPDIR is its enclosing directory
OUT = $(APPDIR)/<appName>
#
# MODULELIB is the library for your target module type: -lsim<module_name>
MODULELIB = -lsim<targetModule>
# TESTS contains the commands that will be executed when `make test` 
# is called using the blockCodeTest.sh script. 
# Individual test commands are separated by ";\"
TESTS = : #;\
#
# End of Makefile section requiring input by user
#####################################################################

OBJS = $(SRCS:.cpp=.o)
DEPS = $(SRCS:.cpp=.depends)

OS = $(shell uname -s)
SIMULATORLIB = $(MODULELIB:-l%=../../simulatorCore/lib/lib%.a)

ifeq ($(GLOBAL_INCLUDES), )
INCLUDES = -I. -I../../simulatorCore/src -I/usr/local/include -I/opt/local/include -I/usr/X11/include
else
INCLUDES = -I. -I../../simulatorCore/src $(GLOBAL_INCLUDES)
endif

ifeq ($(GLOBAL_LIBS), )
	ifeq ($(OS),Darwin)
LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -lGLEW -lglut -framework GLUT -framework OpenGL -L/usr/X11/lib /usr/local/lib/libglut.dylib $(MODULELIB)
	else
LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lm -L/usr/X11/lib  -lglut -lGL -lGLU -lGLEW -lpthread $(MODULELIB)
	endif				#OS
else
LIBS = $(GLOBAL_LIBS) -L../../simulatorCore/lib
endif				#GLOBAL_LIBS

ifeq ($(GLOBAL_CCFLAGS),)
CCFLAGS = -g -Wall -std=c++11 -DTINYXML_USE_STL -DTIXML_USE_STL
	ifeq ($(OS), Darwin)
	CCFLAGS += -DGL_DO_NOT_WARN_IF_MULTI_GL_VERSION_HEADERS_INCLUDED -Wno-deprecated-declarations -Wno-overloaded-virtual
	endif 
else
CCFLAGS = $(GLOBAL_CCFLAGS)
endif

CC = g++

.PHONY: clean all test

.cpp.o:
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

%.depends: %.cpp
	$(CC) -M $(CCFLAGS) $(INCLUDES) $< > $@

all: $(OUT)
	@:

test:
	@$(TESTS)

autoinstall: $(OUT)
	cp $(OUT)  $(APPDIR)

$(APPDIR)/$(OUT): $(OUT)

$(OUT): $(SIMULATORLIB) $(OBJS)
	$(CC) -o $(OUT) $(OBJS) $(LIBS)

ifneq ($(MAKECMDGOALS),clean)
-include $(DEPS)
endif

clean:
	rm -f *~ $(OBJS) $(OUT) $(DEPS)
```

## Meld
VisibleSim supports programs written in [Meld](http://www.cs.cmu.edu/~claytronics/software/meld.html). In that case, the workflow is a bit different from its C++ counterpart. 

_This section explains how to develop and run Meld applications for any type of modular robot._

### Workflow
There is a single application directory for all Meld applications (`applicationsBin/meld`), there is no need to create a separate directory as far for C++ applications. It is fairly easy to develop a Meld application in a few steps:

1. __WRITING__: Create a new `.meld` file in `applicationsBin/meld/programs`
	- Please refer to [this document](https://github.com/flavioc/meld/blob/dev/docs/manual.pdf) for instructions on writing Meld programs.
	- In order to reference interfaces from a certain module family,  the instruction `include #include/<module>.meld` can be used to include one of the provided Meld include files.
2.  __COMPILING__: Compile `programs/<prog>.meld` into `outprogs/<prog>.bb`. 
	- Use  `./compile-meld.sh programs/<prog>.meld`.
3. __RUNNING__: Because there is also a single executable for all modular robots types, you have to provide two additional command line arguments to VisibleSim when running a Meld program:
	- `-p outprogs/<prog>.bb`: Specifies the Meld program to run.
	- `-k <module>`: Specifies the target modular robot type.
		-  _Possible Values: {BB, RB, SB, C2D, C3D}_

__N.B.:__ These instructions only apply to __Modular Robots__, since _MultiRobots_ also support Meld, but have to be used from a separate directory due to them not strictly being a modular robotic systems.

### VisibleSim Predicates
There are a number of action predicates that can be derived in Meld to perform changes in the simulated world, these are the ones currently supported and how to use them:

- `!setColor(Node N, Int r, Int g, Int b, Int i).`: Sets the color of the module `N` to color corresponding to `(r,g,b,intensity)`.
- `!setColor2(Node N, Int C).`: Sets the color of the module `N` to color with identifier `C`. 
- `!tap(Node N).`: Taps module `N` (Schedules a tap event).
- `!moveTo(Node, Int x, Int y, Int z)`: Performs a translation of the module to cell at coordinates `(x,y,z)`.
- `!rotate2D(Node N, Node Pivot?)`: __Not yet implemented__.
- `!rotate3D(Node N, Node Pivot?, ...?)`: __Not yet implemented__.

Furthermore, the following persistent predicates can also be used to each module:

- `!at(Node N, Int x, Int y, Int z)`: Position predicate for a module. 
	- There is an `at` tuple in the database with the current position of the module at all time.
- `!neighbor(Node A, Node B, Int f)`: Present in the module's database if module with identifier `A` has a neighbour of identifier `B` on face corresponding to `f`. 
	- Please refer to the include files in `programs/includes/`, which provide constant identifier for interfaces.
- `!vacant(Node N, Int f)`:  Means that interface `f` is no connected to any module.
	- If there is no `neighbor` tuple in a module's database for its face `f`, then there is a `vacant` tuple instead.
- `!neighborCount(Node N, Int C)`: Indicates the number of neighbours (_i.e._ number of connected interfaces) of a module. 
	- _Constraint:_ `0 <= neighborCount <= NB_INTERFACES`.

## <a name="config"></a>Configuration Files
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

- !`target="x,y,z"`: Specifies the location where the object in pointing at, in the simulated world. 
- !`directionSpherical="θ,φ,r"`: Where `(θ, φ, r)` are the [spherical coordinate](https://en.wikipedia.org/wiki/Spherical_coordinate_system) of the object's direction, `θ`: azimuth angle - `φ`: polar angle - `r`: radial distance.
- !`angle="α"`: _Angular field of view_ of the object in degrees. 

__N.B.:__ NOT necessary if using  _terminal mode_.

#### !`blockList` 

The `blockList` element describes the starting physical position and color of modules (+ extra attributes depending on module type) in the simulated world, as well as their logical identifier for simulation. 
There are several types of children elements that can be used to describe the ensemble, and that can be combined: `block`, `blocksLine`, and `blocksBox`.

```xml
<blockList color="r,g,b" ids="[MANUAL|ORDERED|RANDOM]" idStep="sp" seed="sd">
	<!-- Description of all blocks in simulation -->
	<block position="x,y,z" color="r,g,b" master="true/false" id="i"/>;
	<!-- ... -->
	<blocksLine plane="p" line="l" color="r,g,b" values="00101...1110"/>
	<!-- ... -->
    <blockBox boxOrigin="x,y,z" boxSize="L, l, h" color="r,g,b" />
</blockList>
```
Attributes: 

- `color="r,g,b"`: The default color of the modules. If unspecified, set to dark grey.
- `ids`: Please refer to [Module Identifiers Assignment Schemes](#ids) below.

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

#### Behavioral Customization
Some of VisibleSim's behaviors can be customized so as to let the user tweak some variables of the simulation. 

##### Description
The list of customizations to apply falls under the `customization` XML tag, where each child is a customization groupe that tweaks one specific behavior.

```xml
<customization>
  <rotationDelay multiplier="0.5" />
  <...>
</customization>
```
##### Rotation Speed Tweaking
For now, the only customizable property is the rotation speed of catoms. This is done in the `rotationDelay` element of the `customization` group. The editable attribute is `multiplier`, which takes a float value that will be used as a multiplier to the default rotation **delay** - i.e., a multiplier of 0.5 will result in a rotation twice as fast as normal, and 2.0 twice as slow.

#### <a name="target"></a>Reconfiguration Targets
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
- `format="csg"` : describes a target configuration in term of a combination of geometrical shapes.

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
    bool isInTarget(const Cell3DPosition &pos);
    /**
     * @brief Returns the target color at position pos
     * @param pos position to condiser
     * @return target color at cell p
     */
	const Color getTargetColor(const Cell3DPosition &pos);
	//!< Prints a target to an output stream
    ostream& operator<<(ostream& out,const Target &t);

// BlockCode.h
    /**
     * @brief Loads the next target from the configuration file 
     * into the target attribute by calling Target::loadNextTarget()
     * @return true if a target has been loaded, 
     *         false otherwise (No target remaining in config file)
     */
    static bool loadNextTarget();
```

The `Target` API is quite straighforward:

- The `BlockCode` abstract class contains a `static` pointer to a `target`, hence any user block code will also have access to that variable. It can be accesses using `<blockCodeName>::target`.
- Upon VisibleSim startup, `<blockCodeName>::target` will be loaded with the first occurence of `target` from the configuration file. If none is defined, then `<blockCodeName>::target` will be `NULL`.
- Once a `target` is loaded, the following functions can be used: 
	- `isInTarget`: check whether or not a given cell is part of the target. 
	- `getTargetColor`: get the the target color for a given cell. If not part of the target, `InvalidPositionException` is raised.
	- `<ostream> << <blockCodeName>::target`: prints the target to an output stream.
- Finally, use `<blockCodeName>::loadNextTarget()` to read the next target from the configuration file, and store it into `<blockCodeName>::target`. If there was no additional target defined, this function will return `false`, and `target` attribute will be `NULL`.

#### <a name="userParsing"></a>Parsing Additional User Elements
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

## User Interactions
When running VisibleSim in graphical mode (enabled by default), the user is given a number of ways to interact with the simulated world to perform various actions, as can be seen on the screenshot below.

![VisibleSim UI Screenshot](https://i.imgsafe.org/eee8f96789.png)

### Base Interactions

- <kbd>ctrl</kbd> + <kbd>Left-Click</kbd>: __Block Selection__
	- When performed on a block, selects the clicked block (Show its information and messages on the left interface panel. Selected block is _blinking_).
	- When performed elsewhere, unselect previously selected block.
- <kbd>ctrl</kbd> + <kbd>Right-Click</kbd>: __Display Contextual Menu__
	- Makes the contextual menu appear when on a block.
	- Does nothing otherwise.
- <kbd>r</kbd> / <kbd>R</kbd>: __Simulation Start__
	- <kbd>r</kbd> starts the simulation in realtime mode.
	- <kbd>R</kbd> starts the simulation in fastest mode.
- <kbd>s</kbd>: __Export Screenshot__ (both _.ppm_ and _.jpeg_).
- <kbd>S</kbd>: __Start/End Video Capture__ (_.mkv_, package `ffmpeg` must be installed! `sudo apt-get install ffmpeg`).
- <kbd>Page-Up</kbd>: __Increase Rotation Speed__ (only _Catoms3D_ supported).
- <kbd>Page-Down</kbd>: __Decrease Rotation Speed__ (only _Catoms3D_ supported).
- <kbd>Space</kbd>: __Pause/Resume Simulation__
- <kbd>z</kbd>: __Camera Centering__
	- Centers the camera on the selected block if there is one.
- <kbd>w</kbd> / <kbd>W</kbd>: __Toggle Full Screen__
- <kbd>h</kbd>: __Toggle Help Window__
- <kbd>i</kbd>: __Toggle Console Sidebar__
- <kbd>q</kbd> / <kbd>Q</kbd> / <kbd>esc</kbd>: __Quit simulation__

### Contextual Menu
The contextual menu can be used to interact with a single block from the ensemble, and modify the configuration, which can be exported to en XML file if needed. 

#### General Interactions
Some modules types can provide specific interactions, but these are the base ones :

- `Add block`: Add a block to the selected face of the selected block. If that face does not correspond to any interface, or if the interface is already connected, this option will be greyed out. The `ID` of the added block will be set to the current maximum `ID` of the ensemble incremented by 1. 

- `Delete block`: Remove the selected block from the ensemble after properly disconnecting from its neighbours. A new block can be added to the same position afterwards.

- `Tap`: "Taps" the selected block, which schedules a `TAPEVENT` that can be processed by a customised user handler in `BlockCode`, by overloading the `virtual void onTap(int face);` function. Prints a message to the console by default. Mostly used for debugging. 

- `Save`: Saves the current configuration to a XML configuration file named `config_hh_mm_ss.xml`. All the world, camera, spotlight, and lattice attributes are exported, as well as the positions, color, master and module-specific attributes of the blocks. For more information on how to edit the file to suit your needs, please refer to the [Configuration Files section](#config).

- `Cancel`: Close popup menu. 

#### Catoms2D Interactions
When simulating __Catoms2D__ ensembles, two additional actions can be performed from the contextual menu: 

- `CW Rotation`: __Clockwise Rotation__
	- Rotates the selected block 60º clockwise.

- `CCW Rotation`: __Counter-Clockwise Rotation__
	- Rotates the selected block 60º counter-clockwise.

Both actions perform a rotation of the block corresponding to an angle of one interface (60º). A rotation in any direction is possible if and only if the module has three consecutive vacant interface in that direction. In case one or both rotations are not possible, the options will be greyed-out.

## Debugging
As for many programs, `gdb` can be used to debug VisibleSim programs. However, by default gdb would stop all threads when a breakpoint is reached or a signal has been raised. It is desirable to change this behavior such that only the scheduler thread is stopped when a break occurs, such that the GUI can be inspected and is updated while debugging. 
To do so, we encourage adding the following two lines to your `.gdbinit` file, or to run them before any command after opening gdb:

```
set pagination off
set non-stop on
```

## Clock
In VisibleSim, each `BuildingBlock` is using an independent internal clock, which can be configured to suit the user's needs.

### Clock API
#### Clock Model Assignment
Every `BuildingBlock` has a `clock` member, which is a pointer to an instance of a subclass of the `Clock` abstract class. When a `BuildingBlock` is constructed, it is given a `PerfectClock` by default.

In order to change the clock model of a block, the `BuildingBlock` class provides the following setter: 
```C++
/**
 * @brief Set the internal clock to the clock in parameter
 * @param c clock which the internal clock will be set
 */
void setClock(Clock *c);
```

Where `c` is an instance of one of the clock models detailed in the [Clock Models](#Clock_Models) section. This can be done from the user `BlockCode` in the `startup()` function.

#### Operations
```C++
  /**
   * @brief returns the local time for the simulator time in parameter.
   * @para simTime Simulator time for which the local time is requested.
   * @return local time for simTime
   */ 
  Time getTime(Time simTime);

  /**
   * @brief returns the local time for the current simulator time
   * @return local time for current simulator time
   */ 
  Time getTime();
  
  /**
   * @brief returns the simulator time for the local time in parameter.
   * @para localTime Local time for which the simulator time is requested.
   * @return simulator time for localTime
   */
  Time getSimulationTime(Time localTime);
```

The API for using internal clocks is only made of two fundamental operations, either convert simulator time into local time for a module using `clock->getTime(simTime)`, or perform the reverse operation with `clock->getSimulationTime(moduleTime)`. 

For more convenience, a third function, `clock->getTime()` is provided as a shortcut for getting the local time for the current simulator time.

### <a name="Clock_Models"></a>Clock Models

#### Perfect Clock
By default, the modules are initialised with a _drift-free_ clock model, which means that the global simulator time will always be equal to the local time of the modules.

#### QClock: Quadratic-Model Clock 

Many hardware clock oscillators can be modeled using a quadratic model<sup>_[1]_</sup>:  
![Quadratic Model](https://latex.codecogs.com/png.latex?x%28t%29%20%3D%20%5Cfrac%7B1%7D%7B2%7D%20%5Ccdot%20D%20%5Ccdot%20t%5E2%20&plus;%20y_0%20%5Ccdot%20t%20&plus;%20x_0%20&plus;%20%5Cepsilon%28t%29)

Where `t` is the real-time (i.e. simulator time), `x(t)` is the clock local time, `D` is the frequency drift , `y0` is the frequency offset, `x0` is the time offset, and `ε(t)` is the random noise.

 `QClock` uses this quadratic clock model. Two types of `QClock` are available VisibleSim, namely `GNoiseQClock` and `DNoiseQClock`. They differ by the way the noise `ε(t)` is simulated: 
 
 - In `GNoiseQClock` (Gaussian-noise quadratic-model clock), the noise is simulated using a Gaussian distribution. 
 - The method used in `DNoiseQClock` is inspired by the work presented in _[2]_. 
    - In `DNoiseQClock` (data-noise quadratic-model clock), the noise is replayed from data. We experimentally collected noise data for some BlinkyBlocks clocks during four hours. BlinkyBlocks clocks are driven by `XMEGA_RTC_OSC1K_CRC` calibrated RC oscillators with  a resolution of 1 millisecond and an accuracy of 1% accuracy (10 000 ppm) at 3V and 25°C. 
 	- Users can easily instantiate such clocks using the following function (Only works until simulator time reaches four hours): 
```C++
  /**
   * @brief Create a DNoiseQClock that simulates the XMEGA_RTC_OSC1K_CRC 
   * hardware clock. XMEGA_RTC_OSC1K_CRC: Calibrated RC Oscillator, 1 ms 
   * 1% accuracy (10 000 ppm) at 3V and 25°C.
   * @para seed seed use to randomly select the noise signal
   */ 
  static DNoiseQClock* createXMEGA_RTC_OSC1K_CRC(ruint seed);
```

For quadratic-model clocks, converting simulator time into local time using `clock->getTime(simTime)` is a relatively straightforward and efficient operation. On the other hand, users should be aware that the reverse operation, perfomed with `clock->getSimulationTime(moduleTime)`, is quite costly: This operation is perfomed using a guided search mechanism as the random noise is not constant over time.

_[1] David W Allan. Time and frequency(time-domain) characterization, estimation, and prediction of precision clocks and oscillators. IEEE transactions on ultrasonics, ferroelectrics, and frequency control, 34(6):647–654, 1987.  
[2] Ring, F., Nagy, A., Gaderer, G., & Loschmidt, P. (2010, November). Clock synchronization simulation for wireless sensor networks. In Sensors, 2010 IEEE (pp. 2022-2026). IEEE._

## <a name="autotest"></a>Automated BlockCode Testing
We have designed a way for contributors to effortlessly ensure that the behaviour of VisibleSim is not altered, when some changes are made to the core of the simulator. The idea is that every previously working BlockCode is tested for regression, and returns a simple console output to the user, either __PASS__ if test succeeded, or __FAILED__ if some issues have to be investigated. 

### Procedure
This testing procedure is based on the assumption that a BlockCode execution is valid if its output (in term of end-of-algorithm configuration and individual block physical properties) is identical to the _expected output_, that has been previously defined.

A single BlockCode can have several regression tests, thus every test from a single application directory needs to be identified by a unique string identifier (e.g. `testSquare`), the `testID`. 

#### Utilities

We use exports of configuration files at the end of algorithm as output, and the `utilities/blockCodeTest.sh` script to perform the test and decide the result. The usage information for the script are the following:

```sh
Usage: ./utilities/blockCodeTest.sh <test-ID> <path-to-blockCode-binary> <VisibleSim-arguments>
Example: ./utilities/blockCodeTest.sh bbCycle1 ../applicationsBin/bbCycle -c config123.xml
Test-ID can be used to distinguish between 2 control XML files from the same directory
```

In fact, you only need to provide a `testID`, followed by the usual VisibleSim arguments you would normally  use to execute your BlockCode. 

In order to automate testing, we implemented the `make test` Makefile directive, which can be called recursively from the root Makefile to perform regression testing on all individual applications. In order to do so, all you have to do is edit the `TESTS` command in user-editable section of the application directory's Makefile. It has to contain a list of testing commands using the `blockCodeTest.sh` script that have to be executed.

By default, this variable only contains a shell `not`, and does nothing.
```makefile
# TESTS contains the commands that will be executed when `make test` is called
TESTS = : #;\
```
Example of a `TESTS` variable containing several tests (Use `;\` to separate individual commands):
```makefile
# TESTS contains the commands that will be executed when `make test` is called
# $(OUT) is the executable
TESTS = ../../utilities/blockCodeTest.sh meldbb $(OUT) -c configs/configBB.xml -p outprogs/rainbow.bb -k BB ;\
	../../utilities/blockCodeTest.sh meldsb $(OUT) -c configs/configSB.xml -p outprogs/rainbowSB.bb -k SB ;\
# ...
	../../utilities/blockCodeTest.sh meldc3d $(OUT) -c configs/configC3D.xml -p outprogs/rainbowC3D.bb -k C3D
```

Which will produce the following output after a call to `make test`, if all is good:
```sh
BlockCodes Regression Testing:
meldbb:				[PASS]
meldsb:				[PASS]
meldrb:				[PASS]
meldc2d:			[PASS]
meldc3d:			[PASS]
```

_The complete testing process is detailed below._

#### Control Configuration Export
Only has to be done once, this is when the user defines what the expected output of the BlockCode is, given an input file. To generate the control XML file, the user can execute VisibleSim with the `-g` option, that will automatically export the configuration to an XML file named `.confCheck.xml`, when all scheduler events have been processed. 

Then, this export file needs to be renamed to `.controlConf_<testID>.xml` in `applicationsBin/<testedApp>/`, to be used as control configuration for the test `testID`  of the `testedApp` BlockCode.

#### Regression Testing
In order to test for regression, the BlockCode is executed with the exact same parameters as in the last section, and a new end-of-algorithm configuration is exported. A few different scenarios can occur:

1. __Runtime Exception / Error__: If an uncaught exception has been thrown, or if a system error occurred, the test status will be __FAILED__.
2. __Terminal Configuration Mismatch__: If when comparing the control configuration and the newly exported one (using a `diff`), the output is different, then __regression__ as occurred, and the status of the test is __FAILED__.
3. __Terminal Configuration Match__: Inversely, if the two XML files are identical, then the test succeeded, and __PASS__ is shown. 
4. (__Missing Control File__): If when running the script, no control configuration currently exists, then user will be asked to export one interactively, in order for the test to proceed.
 
  __N.B.__: Due to the testing procedure itself, it is not possible to test algorithms that never end, since no terminal configuration can be exported.


@author P. Thalamy - pierre.thalamy@femto-st.fr
