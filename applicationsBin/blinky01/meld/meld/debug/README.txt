
<==============================================================>

DEBUGGER MANUAL

Please email for questions:
Dave Campbell --dncswim76@gmail.com
7/19/2013

<==============================================================>


********************************************************************************
A.   HOW TO RUN THE DEBUGGER
********************************************************************************

     I. MPI debugging MODE:

         To run the debugger in mpi debugging mode, the program must be compiled
     using the mpic++ compiler.  Once compiled, you may execute the program
     by entering:

        mpiexec -n <# of processes> ./meld -c sl -f <meld program> -D MPI

     This will set the debugger into MPI debugging mode with the -D flag, given
     the specified number of processes.

     NOTE:  the number of VMs run will be <# of processes> - 1 because the 0th
     process is reserved for the debugging controller that shows the debugging
     prompt.  For example, -n 3 will be running two VMs in debugging mode.

     NOTE: running with -n 2 will be the same as running in VM debugging mode.

     NOTE: To be able to run the boost mpi, you will need to download the
     'openmpi-bin' package.

     II. SIMULATOR Debugging MODE

        Within the Simulator when executing the VMs, the -D SIM flag should be
        appended.  The path in the config.xml file should match yours and the
        debug flag within the xml must be set to true


********************************************************************************
B.  THE DEBUGGING INTERFACE
********************************************************************************

    I. Setting Breakpoints

        A Breakpoint has five different types:
                     1. factDer (fact Derivation)
                     2. factCon (fact Consumation)
                     3. factRet (fact Retraction)
                     4. sense
                     5. action

         To specify that you want to run the break point, type in the command:

            >break <type>
                ex.
                >break factDer

                This will tell all nodes in the system to break when a fact
                is derived.

         You can also specify the name of the type by inserting a colon after
         the type.

            >break <type>:<name>
                ex.
                >break factCon:iterations

                This will tell all nodes in the system to break when a fact with
                the name iterations has been consumed.

         Finally, you can specify the node that you want to set the break point
         at by inserting it after the '@' character.

            >break <type>:<name>@<node#>
                ex.
                >break factRet:elem@3

                This will tell the certain node that fact name elem as been
                retracted at node 3.

            OR

            >break <type>@<node#>
                ex.
                >break sense@3

                This will tell node 3 to break if it has any kind of sense.


      II.  Dumping The System

          To dump the contents of the system:

             >dump all
             -This will dump all the nodes in the system.

             >dump <node#>
             -This will dump the contents of a certain node.

      III.  Removing breakpoints

          To remove a breakpoint, enter:

          >rm <specification>
          The specification follows that exact same way you would decribe a
          a breakpoint.
            ex.  rm factDer:iterations@3

      IV.  Printing the breakpoint list

          To print your currently input breakpoints, enter:

             >print

       V.   Quitting the debugger

          To leave to debugger,

             >quit

       VI. To continue execution of a paused VM:

             >run

             OR

             >continue

       VII.  changing the debugging modes

            To be mode verbose:

            >mode V

            To go into serialization mode:
            !!CANT USE IN SERIALIZATION MODE
            >mode S

            To do both:

            >mode SV


      VII.  IMPORTANT NOTES

          -When the debugger is in mpi debugging mode, the feedback will also
          tell you which VM the information came from.

          -The types sense, action, and block are really only specific to
            the simulation debugger.  If they are input in another mode,
            they will be ignored, and the program will run as if
            they are not there.

       VIII.  SIMULATION DEBUGGING

           -When in simulation debugging mode, before your type run into
            the debugger, you must first press 'r' within the simulation
            to let the simulation know to start.  Then you can tell the
            system to unpause.
           -if you add a block while the system is paused, it will not show up
            until the next time you type run or continue

           -There are only specified names that will work for sense and action:
                 sense:  'tap','accel','shake'
                 action: 'red','orange','yellow'... (only VM colors)
                 ex.  break action:orange@3



           -If the simulator is running and you want to stop it for debugging mode,
            you can press p anytime while the system is in exection (after continue
            or run command) to stop the system to probe.  This is most useful
            for when the blinky blocks have reached a sort of equilibrium.
            !!!IMPORTANT:  This MUST be done in the simulator window, not command line.

********************************************************************************
C.  HOW THE DEBUGGER IS IMPLEMENTED
********************************************************************************

            When a breakpoint is input, the debugger will enter it into a list
        of breakpoints. As the program executes, at specific locations, it will
        trap the program into a loop until the debugging controlling thread
        breaks it out. while paused, the debugger can asto print the system
        as well as print out other information like the fact list.

            The function runBreakPoint acts as a filter that checks to see if
        a breakpoint was reached and is inserted into the VM code.

            In MPI debugging mode, the debugger uses the boost interface to send
        messages across multiple VMs.  When sending to all nodes, the debugger
        will request to the api to broadcast the signal as opposed to just one
        signal.  Each separate VM will have its own break point list.  When the
        api gets a debug message, it will push it into the debugger messageQueue.

            Also, in the MPI debugging mode, PROCESS 0 is reserved as the
        controlling process.  So the VMs will be numbered from 1 to N-1.

            In SIM debugging mode,  there is a controlling THREAD that requests
        the simulator to send messages over the api.

                         When in simulating mode, the debugger expects to
                         recieve an expected number if messages back to ensure
                         accurate synchronization of the input prompt.

        For further inquery of how the debugger is implemented see:

              debug/debug_handler.cpp
              debug/debug_prompt.cpp
              debug/debug_list.cpp

              api/mpi.cpp

              vm/exec.cpp (where the factDer and factCon break points are set)

              vm/state.cpp (where the factRet break point is placed)

              process/machine.cpp (where action breakpoints are placed)

             sched/sim.cpp (where sense breakpoints are placed)
