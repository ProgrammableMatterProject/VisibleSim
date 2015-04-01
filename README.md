VisibleSim
==================

Private version of VisibleSim and its applications. VisibleSim is a general simulator for modular 
robots.

## Installation

Dependencies:
 - Boost C++ Libraries >= 1.47.0
 - GLUT
 - GLEW

**NOTE: video card drivers have to be correctly installed. Otherwise, VisibleSim crashes when moving the mouse on the simulated modules.**

### Mac Installation

 1. Install dependencies:

```
brew install boost --with-mpi --without-single
brew install freeglut
brew install glew
```

 2. Uncomment mac-specific `GLOBAL_LIBS` in `Makefile`
 3. make

### Ubuntu Installation

 1. Intall dependencies:

``` 
sudo apt-get install libboost-all-dev
sudo apt-get install freeglut3-dev
sudo apt-get install libglew-dev
```

 2. make

## Applications

### Application examples

Application examples are available in `applicationsSrc/` folder. For instance, in `bbCycle`, blocks
change their color every two seconds using their own local clock. Since local clocks drift apart,
the color change are very quickly desynchronized.

### Implementing a new application

To implement a new application, add a folder in `applicationsSrc/`, program the application (see
Application examples and the documentations available in `doc/`), add the folder name in `SUBDIR` 
macro in `applicationsSrc/Makefile`.

### Running an application

VisibleSim supports both c++ applications and Meld appplications (currently, only Blinky Block
environment supports Meld programming). Regardless of the type of application you want to run, you 
must press '<kbd>SHIFT</kbd> + <kbd>r</kbd>' (real-time mode) or '<kbd>SHIFT</kbd> + 
<kbd>R</kbd>' (fastest mode) in the simulator window to launch the 
simulation.

#### Running a c++ application

To execute an application named `myApp`, compile it with `make` in the root folder, `cd` into
`applicationsBin/myApp`, create an appropriate xml configuration file (see other configuration
files in `applicationsBin/` folders, and run `./myApp -c configuration.xml`. By default, `config.xml`
is loaded, but you can provide another configuration file with `-c` option.

#### Running a Meld application

To execute meld programs on VisibleSim, ensure that `blinky01` application was compiled (`blinky01` 
had belong to `SUBDIR` macro in `applicationsSrc/Makefile` when the last `make` in the root folder 
was performed), `cd` into `applicationsBin/blinky01/` and run

```
./compile-meld.sh program.meld // to compile your Meld program into program.m
./blinky01 -p program.m -c configuration.xml // to run your meld program 
```

If you want to use the debugger, run the simulator with `-D` option
(`./blinky01 -p program.m -c configuration.xml -D`). The simulator window should appear and the 
console should read:
```
SIMULATION DEBUGGING MODE -- type help for options
>
```

**NOTE: you must press '<kbd>SHIFT</kbd> + <kbd>r</kbd>' or '<kbd>SHIFT</kbd> + <kbd>R</kbd>' in the simulator window before you can start running commands from the console.**

After pressing '<kbd>SHIFT</kbd> + <kbd>r</kbd>' or '<kbd>SHIFT</kbd> + <kbd>R</kbd>' in the simulation window, typing `run` in the console should start your program.
