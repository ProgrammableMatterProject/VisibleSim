We have lots of options for the vm at this point and we need a way to
distinguish them so that we can create vm executables which are
appropriate for each target without having a lot of cruft in them.
This is especially important for the bbhw version which needs to have
as small a footprint as possible.

The two main axis are:

(S) shared address space V. (D) distributed address space

and

(1) 1 node per thread V. (M) many nodes per thread

uniproc: SM
multi-core: SM
mpi: DM
bbsim: D1
bbhw: D1

The M versions:
- should have all the different thread schedules.
- need some locking on the DB
- require the node id to be stored in the DB

The 1 versions:
- only need the serial scheduler
- don't need to store the nodeid in the DB
- require no locking on the DB

In addition, we have the option for the debugger, whether it is
simulated or not, whether it is deterministic or not, whether it is
running as a web backend or not.

We need to be able to identify what the executable is, make different
executables, and have a test-suite that tests the various verions.

Thoughts?

