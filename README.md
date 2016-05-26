VisibleSim
==================

### Merge Branch

- 26/05/16: Merged with Refactoring
- 26/05/16: Merged with catoms2D

Dependencies:
 - Boost C++ Libraries >= 1.47.0
 - GLUT
 - GLEW
 - SBCL - to compile Meld programs

### Mac Installation

1\. Install dependencies:
```
brew install boost --with-mpi --without-single
brew install freeglut
brew install glew
brew install sbcl
```
2\. make

**NOTE: VisibleSim uses boost::interprocess::interprocess_semaphore. Unfortunately, its implementation in Boost 1.56 is buggy on MacOS 10.9.5. VisibleSim compiles but throws an exception at runtime saying the function is not implemented. Boost 1.56 actually implementes interprocess_semaphore using POSIX unnamed semaphores, which are not implemented in MacOS 10.9.5. To fix that bug, edit `/usr/local/include/boost/interprocess/sync/interprocess_semaphore.hpp` and comment the following lines to make Boost use SPIN semaphore (see the official ticket [boost1.56-ticket] for more details):**
 ```
#if !defined(BOOST_INTERPROCESS_FORCE_GENERIC_EMULATION) && \
   (defined(BOOST_INTERPROCESS_POSIX_PROCESS_SHARED) && defined(BOOST_INTERPROCESS_POSIX_NAMED_SEMAPHORES))
   #include <boost/interprocess/sync/posix/semaphore.hpp>
   #define BOOST_INTERPROCESS_USE_POSIX
```

### Ubuntu Installation

1\. Intall dependencies:
```
sudo apt-get install libboost-all-dev
sudo apt-get install freeglut3-dev
sudo apt-get install libglew-dev
sudo apt-get install sbcl
```
2\. make

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
must press '<kbd>r</kbd>' (real-time mode) or '<kbd>R</kbd>' (fastest mode) in the simulator window to launch the 
simulation.

#### Running a c++ application

To execute an application named `myApp`, compile it with `make` in the root folder, `cd` into
`applicationsBin/myApp`, create an appropriate xml configuration file (see other configuration
files in `applicationsBin/` folders, and run `./myApp -c configuration.xml`. By default, `config.xml`
is loaded, but you can provide another configuration file with `-c` option.

#### Running a Meld application

Meld programs only run on Blinky Blocks for now. To execute a Meld program on VisibleSim, ensure that `blinkyMeld` application is compiled (`blinkyMeld` 
had belong to `SUBDIR` macro in `applicationsSrc/Makefile` when the last `make` in the repository 
root folder was performed). Then, `cd` into
`applicationsBin/blinkyMeld/` and run:

```
./compile-meld.sh [path to your Meld program] // to compile your Meld program.
./blinkyMeld -c configuration.xml // to run your Meld program.
```

Press '<kbd>r</kbd>' to run the simulation.

Meld program examples are available in `applicationsBin/blinkyMeld/meld/programs`. All programs were not tested.

[boost1.56-ticket]:https://svn.boost.org/trac/boost/ticket/11154
