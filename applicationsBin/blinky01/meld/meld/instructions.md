Installation Instructions
=========================

This document describes how to install the meld bytecode VM.
Instructions on how to install the meld compiler (cl-meld) can be found
in the cl-meld repository.


Step 1: Install dependencies
----------------------------

The meld VM has two explicit dependencies:

1. MPI (either MPICXX or open-mpi)
2. Boost (must include boost-thread, boost-system-mt, boost-date-time-mt, boost-regex-mt, boost-serialization-mt and boost-mpi-mt)

### For Mac OS

Installing the dependencies on Mac is trivial with the [homebrew](http://brew.sh/) package management tool:
```
brew install open-mpi
brew install boost --with-mpi --without-single
```

### For Linux (ubuntu 64-bit)

Use apt-get to install dependencies on Ubuntu:
```
sudo apt-get install mpi-default-dev libboost-all-dev libreadline-dev
```

Step 2: Compile the meld VM
---------------------------

First, download the source code:
```
git clone https://github.com/claytronics/meld.git
```
Then compilation should be trivial:
```
cd meld
make
```

Step 3: Test the installation
-----------------------------

To run a program:
```
$ ./meld -f <byte code file> -c sched
```

Where sched is either sl, th or thp. Both tl and tlp need the number of threads, thus to use 4 threads with tl you do:
```
./meld -f file.m -c tl4
```

To measure execution time, add `-t` to the command line. To see the final contents of the database, add `-d` or `-s`.  Use `./meld -h` for help.
