VisibleSim
==================
Dependencies:
 - Boost C++ Libraries >= 1.47.0 (If using Meld Process only)
 - GLUT
 - GLEW
 - SBCL - to compile Meld programs
 - MUPARSER - to use CSG

## Installation

### Installing Submodules
There are a number of other repositories included as [git submodules] in VisibleSim and used as utilities (e.g. a configuration generator). They need to be fetched from their respective repositories before they can be used. This can be done with the following commands, from the project's root:
```shell
git submodule init
git submodule update
```

### Mac Installation

1\. Install dependencies:
```shell
brew install boost --with-mpi --without-single
brew install freeglut
brew install glew
brew install sbcl
```
2\. `make`

**NOTE: VisibleSim uses boost::interprocess::interprocess_semaphore if using _Meld Process_. Unfortunately, its implementation in Boost 1.56 is buggy on MacOS 10.9.5. VisibleSim compiles but throws an exception at runtime saying the function is not implemented. Boost 1.56 actually implementes interprocess_semaphore using POSIX unnamed semaphores, which are not implemented in MacOS 10.9.5. To fix that bug, edit `/usr/local/include/boost/interprocess/sync/interprocess_semaphore.hpp` and comment the following lines to make Boost use SPIN semaphore (see the official ticket [boost1.56-ticket] for more details):**
```C++
#if !defined(BOOST_INTERPROCESS_FORCE_GENERIC_EMULATION) && \
   (defined(BOOST_INTERPROCESS_POSIX_PROCESS_SHARED) && defined(BOOST_INTERPROCESS_POSIX_NAMED_SEMAPHORES))
   #include <boost/interprocess/sync/posix/semaphore.hpp>
   #define BOOST_INTERPROCESS_USE_POSIX
```

### Ubuntu Installation

1\. Install dependencies:
```shell
sudo apt-get install libboost-all-dev
sudo apt-get install freeglut3-dev
sudo apt-get install libglew-dev
sudo apt-get install sbcl
sudo apt-get install libmuparser-dev
```
2\. `make`

## Getting Started
A __User Manual__ and a __Technical Reference (_Coming Soon_)__ are available in the `doc` directory, to get users and contributors started with VisibleSim.

[boost1.56-ticket]:https://svn.boost.org/trac/boost/ticket/11154
[git submodules]:https://git-scm.com/book/en/v2/Git-Tools-Submodules
