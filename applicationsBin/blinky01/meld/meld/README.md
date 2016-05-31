meld VM
=======

This is a C++ bytecode VM for Meld.


Dependencies
-------------
  - Boost
    - MPI (optional)
    - Threads
  - libreadline
    - A compiler
      - G++ 4.4/4.2, maybe 4.6
      - LLVM/Clang also works

Execution:
	meld-bbsim: for running under visibleSim
	meld-mpi: for running under mpi
	meld -a bbsim or meld -a mpi will invoke the correct executable

	Use -D to enter debugging mode
	Use ./meld -h for help

Installation/Instructions
-------------------------
See [Instructions.md](https://github.com/claytronics/meld/blob/master/instructions.md)


Source Tree Overview
--------------------

### Directories:
   - db: implements the database, including facts (tuples) and nodes
   - external: implements the available external functions (callable in meld programs)
   - mem: memory allocator for objects used during execution. This improves multithreaded performance since the default allocator is sequential.
   - process: the machine class starts the execution of the virtual machine (program argument, scheduler type and number of threads). There are also MPI management classes.
   - queue: many types of queues here, including priority queues. It's all template code.
   - runtime: classes for list and string objects used for meld objects.
   - sched: sched::base is the base class for all the available schedulers.
      A scheduler implements a way of executing meld. The simplest scheduler is sched/serial for sequential execution.
   - stat: code for generating execution statistics.
   - thread: multithreaded schedulers.
   - ui: manages communication with browser interface.
   - utils: utilities.
   - vm: the core of the virtual machine, executes VM instructions.

### Important files:
   - meld.cpp: main meld program.
   - server.cpp: server program for using the web interface.
