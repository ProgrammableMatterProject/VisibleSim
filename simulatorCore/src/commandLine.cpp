#include <iostream>
#include <cstdlib>

#include "commandLine.h"
#include "scheduler.h"
#include "openGlViewer.h"

#define CMD_LINE_UNDEFINED -1

void CommandLine::help() {
   cerr << "VisibleSim options:" << endl;
   cerr << "\t -f \t\t\tfull screen" << endl;
   cerr << "\t -p <name>\t\tprogram file (Meld for instance) (Meld in only supported on Blinky Blocks for now)" << endl;
   cerr << "\t -D \t\t\tdebugging mode (used in Meld only)" << endl;
   cerr << "\t -c <name>\t\txml configuration file" << endl;
   //cerr << "\t -r \t\trun realtime mode on startup" << endl;
   //cerr << "\t -R \t\trun fastest mode on startup" << endl;
   //cerr << "\t -t \t\tterminal only (no graphic)" << endl;
   //cerr << "\t -i \t\tprint world informations (size, diameter, centers etc.)" << endl;
   /*cerr << "\t -g <topology code> <n or d>\t\t generate a random configuration: random 2d " << 
   TOPOLOGY_RANDOM_2D << ", random 3d " << TOPOLOGY_RANDOM_3D << ", line " << TOPOLOGY_LINE <<
   	", grid " << TOPOLOGY_GRID << ", torus " << TOPOLOGY_TORUS <<", cube" << TOPOLOGY_CUBE << endl;
   */
   //cerr << "\t -s <length>\tgrid side lentgh (square or cubic grid)" << endl;
   cerr << "\t -m <VMpath>:<VMport>\tpath to the MeldVM directory and port" << endl;
   cerr << "\t -h \t\t\thelp" << endl;
   exit(EXIT_SUCCESS);
}

CommandLine::CommandLine(int argc, char *argv[]) {
	schedulerMode = CMD_LINE_UNDEFINED;
	topology = CMD_LINE_UNDEFINED;
	topologyParameter = CMD_LINE_UNDEFINED;
	meldDebugger = false;
	terminalOnly = false;
	stats = false;
        vmPort = 0;
        vmPath = "";
	configFile = "config.xml";
	read(argc,argv);
}

void CommandLine::read(int argc, char *argv[]) {
    /* Reading the command line */
    argv++;
    argc--;
    while ( (argc > 0) && (argv[0][0] == '-')) {
        switch(argv[0][1]) {
        case 'p':   {
            //if (programPath != "")
            //   help();
            if (argc < 1) { 
                help();
            }
            programPath = argv[1];
            argc--;
            argv++;
        } break;
        case 'm': {             // MeldVM Path and Port
            stringstream vm(argv[1]); // <vmPath:vmPort>
            cerr << &argv[1] << endl;
            string portStr;
            std::getline(vm, vmPath, ':');
            std::getline(vm, portStr, ':');
            try {
                vmPort = stoi(portStr);
            } catch(std::invalid_argument&) {
                cerr << "error: MeldVM port must be a number!" << endl;
                exit(EXIT_FAILURE);
            }
            
            argc--;
            argv++;            
        } break;
        case 'D': {
            meldDebugger = true;
        } break;
            /*case 'r': {
              schedulerMode = SCHEDULER_MODE_REALTIME;
              }
              break;
              case 'R': {
              schedulerMode = SCHEDULER_MODE_FASTEST;
              }
              break;
              case 'a' : {
              if (argc < 1) { 
              help();
              }
              topology = atoi(argv[1]);
              argc--;
              argv++;
              if (argc < 1) { 
              help();
              }
              topologyParameter = atoi(argv[1]);
              argc--;
              argv++;
              } 
              break;*/
        case 'c': {
            // Configuration file, already managed in Simulator constructor
            if (argc < 1) {
                cerr << "Provide a configuration file after -c" << endl;
                help();
            }
            configFile= argv[1];
            argc--;
            argv++;
        } break;
            /*case 't': {
              terminalOnly = true;
              }
              break;
              case 'i': {
              stats = true;
              }
              break;
              // TODO: grid size
              case 's': {
              argc--;
              argv++;
              if (argc < 1) {
              cerr << "Provide a grid size after -s" << endl;
              help();
              }
              gridSize = atoi(argv[1]);
              } break;*/
        case 'f' : {
            //fullScreen = true;
            GlutContext::setFullScreenMode(true);
        } break;
        default:
            help();
        }
        argc--;
        argv++;
    }    
}

bool CommandLine::randomWorldRequested() {
	return topology != CMD_LINE_UNDEFINED;
}
