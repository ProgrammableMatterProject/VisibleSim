#include <iostream>
#include <cstdlib>

#include "commandLine.h"
#include "openglViewer.h"

void CommandLine::help() {
    cerr << "VisibleSim options:" << endl;
    cerr << "\t -f \t\t\tfull screen" << endl;
    cerr << "\t -p <name>\t\tprogram file (Meld for instance)" << endl;
    cerr << "\t -D \t\t\tdebugging mode (used in Meld only)" << endl;
    cerr << "\t -c <name>\t\txml configuration file" << endl;
    cerr << "\t -r \t\trun realtime mode on startup" << endl;
    cerr << "\t -R \t\trun fastest mode on startup" << endl;
    cerr << "\t -x \t\tterminate simulation when scheduler ends" << endl;
    //cerr << "\t -t \t\tterminal only (no graphic)" << endl;
    //cerr << "\t -i \t\tprint world informations (size, diameter, centers etc.)" << endl;
    /*cerr << "\t -g <topology code> <n or d>\t\t generate a random configuration: random 2d " << 
      TOPOLOGY_RANDOM_2D << ", random 3d " << TOPOLOGY_RANDOM_3D << ", line " << TOPOLOGY_LINE <<
      ", grid " << TOPOLOGY_GRID << ", torus " << TOPOLOGY_TORUS <<", cube" << TOPOLOGY_CUBE << endl;
    */
    //cerr << "\t -s <length>\tgrid side lentgh (square or cubic grid)" << endl;
    cerr << "\t -s [<maximumDate> | inf] \tScheduler mode:\tBy default, stops when event list is empty\n"
         << "\t\t maximumDate (ms) : the scheduler will stop when even list is empty, or when the maximum date is reach\n"
         << "\t\t inf : the scheduler will have an infinite duration and can only be stopped by the user" << endl;
    cerr << "\t -m <VMpath>:<VMport>\tpath to the MeldVM directory and port" << endl;
    cerr << "\t -h \t\t\thelp" << endl;
    exit(EXIT_SUCCESS);
}

CommandLine::CommandLine(int argc, char *argv[]) {
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
                cerr << "error: No meld program provided after -p" << endl;
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
                help();
                exit(EXIT_FAILURE);
            }
            
            argc--;
            argv++;            
        } break;
        case 'D': {
            meldDebugger = true;
        } break;
        case 'r': {
            if (schedulerMode == CMD_LINE_UNDEFINED)
                schedulerMode = SCHEDULER_MODE_REALTIME;
            else {
                cerr << "error: -r and -R options cannot be enabled at the same time" << endl;
                help();
            }
                
        } break;
        case 'R': {
            if (schedulerMode == CMD_LINE_UNDEFINED)                           
                schedulerMode = SCHEDULER_MODE_FASTEST;
            else {
                cerr << "error: -r and -R options cannot be enabled at the same time" << endl;
                help();
            }
        } break;
        case 'x': {
            schedulerAutoStop = true;
        } break;

            // case 'a' : {
            // if (argc < 1) { 
            // help();
            // }
            // topology = atoi(argv[1]);
            // argc--;
            // argv++;
            // if (argc < 1) { 
            // help();
            // }
            // topologyParameter = atoi(argv[1]);
            // argc--;
            // argv++;
            // } 
            // break;
        case 'c': {
            // Configuration file, already managed in Simulator constructor
            if (argc < 1) {
                cerr << "error: No configuration file provided after -c" << endl;
                help();
            }
            configFile= argv[1];
            argc--;
            argv++;
        } break;
        case 's': {
            char *p; // Used to check if argv[1] is a number, the maximum date
            maximumDate = strtol(argv[1], &p, 10);
            cout << "maximumDate : " << maximumDate << " | argv[1] = " << argv[1] << endl;
                
            if (strcmp(argv[1], "inf") == 0)
                schedulerLength = SCHEDULER_LENGTH_INFINITE;
            else if (!*p) {      // P is pointing to null, hence the number in argv[1] has been entirely read
                // argv[1] is a number
                schedulerLength = SCHEDULER_LENGTH_BOUNDED;
            } else {
                cerr << "error: Found unknown parameter after option -s. Expected <MaximumDate> or \"inf\""
                     << endl;
                help();
            }
            
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
