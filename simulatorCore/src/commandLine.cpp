#include "commandLine.h"
#include "scheduler.h"
#include <iostream>
#include <cstdlib>

#define CMD_LINE_UNDEFINED -1

void CommandLine::help() {
   cerr << "VisibleSim options:" << endl;
   cerr << "\t -f <name>\tprogram file (meld for instance)" << endl;
   cerr << "\t -D \t\tdebugging mode (used in Meld only)" << endl;
   cerr << "\t -c <name>\txml configuration file" << endl;
   cerr << "\t -r \t\trun realtime mode on startup" << endl;
   cerr << "\t -R \t\trun fastest mode on startup" << endl;
   cerr << "\t -t \t\tterminal only (no graphic)" << endl;
   //cerr << "\t -g <topology code> <n or d>\t\t generate a random configuration: random 2d " << 
   //TOPOLOGY_RANDOM_2D << ", random 3d " << TOPOLOGY_RANDOM_3D << ", line " << TOPOLOGY_LINE <<
   //	", grid " << TOPOLOGY_GRID << ", torus " << TOPOLOGY_TORUS <<", cube" << TOPOLOGY_CUBE << endl;
   //cerr << "\t -t \t\tterminal mode " << endl;
   cerr << "\t -h \t\thelp" << endl;
   exit(EXIT_SUCCESS);
}

CommandLine::CommandLine(int argc, char *argv[]) {
	schedulerMode = CMD_LINE_UNDEFINED;
	topology = CMD_LINE_UNDEFINED;
	topologyParameter = CMD_LINE_UNDEFINED;
	meldDebugger = false;
	terminalOnly = false;
	read(argc,argv);
}

void CommandLine::read(int argc, char *argv[]) {
 /* Reading the command line */
   argv++;
   argc--;
   while ( (argc > 0) && (argv[0][0] == '-')) {
      switch(argv[0][1]) {
         case 'f':   {
            if (programPath != "")
               help();
            programPath = argv[1];
            argc--;
            argv++;
         }
         break;
         case 'D': {
            meldDebugger = true;
         }
         case 'r': {
            schedulerMode = SCHEDULER_MODE_REALTIME;
         }
         break;
         case 'R': {
            schedulerMode = SCHEDULER_MODE_FASTEST;
         }
         break;
         case 'a' : {
			topology = atoi(argv[1]);
			argc--;
			argv++;
			topologyParameter = atoi(argv[1]);
			argc--;
			argv++;
		 }
         break;
         case 'c': {
            // Configuration file, already managed in Simulator constructor
            argc--;
            argv++;
         }
         break;
         case 't': {
			 terminalOnly = true;
		 }
		 break;
          // TODO: grid size
         /*case 's': {
			argc--;
			argv++;
			size = atoi(argv[1]);
		 }
         break;*/
         default:
            help();
      }
   argc--;
   argv++;
   }
	
}
