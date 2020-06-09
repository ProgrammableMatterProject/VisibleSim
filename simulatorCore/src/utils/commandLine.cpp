#include "utils/commandLine.h"

#include <iostream>
#include <cstdlib>

#include "stats/statsIndividual.h"
#include "gui/openglViewer.h"
#include "base/simulator.h"
#include "utils/trace.h"

void CommandLine::help() const {
    cerr << TermColor::BWhite << "VisibleSim options:" << TermColor::Reset << endl;
    cerr << "\t " << TermColor::BMagenta << "-f " << TermColor::Reset
         << "\t\t\tFull screen mode" << endl;
    cerr << "\t " << TermColor::BMagenta << "-p <program>" << TermColor::Reset
         << "\t\tPath to a Meld program file (Meld only)" << endl;
    cerr << "\t " << TermColor::BMagenta << "-D " << TermColor::Reset
         << "\t\t\tDebugger mode (Meld only)" << endl;
    cerr << "\t " << TermColor::BMagenta << "-c <config>" << TermColor::Reset
         << "\t\tPath to the XML configuration file" << endl;
    cerr << "\t " << TermColor::BMagenta << "-r " << TermColor::Reset
         << "\t\t\tRun realtime mode on startup: simulation time reflects real time elapsed" << endl;
    cerr << "\t " << TermColor::BMagenta << "-R " << TermColor::Reset
         << "\t\t\tRun fastest mode on startup: scheduler executes all events as fast as possible" << endl;
    cerr << "\t " << TermColor::BMagenta << "-x " << TermColor::Reset
         << "\t\t\tTerminate simulation when scheduler ends (has no effect in terminal mode)" << endl;
    cerr << "\t " << TermColor::BMagenta << "-t " << TermColor::Reset
         << "\t\t\tTerminal mode only (no graphical output)" << endl;
    cerr << "\t " << TermColor::BMagenta << "-s [<maxDate> | inf] " << TermColor::Reset
         << "\tScheduler mode:\t(Default) Stop simulation when event list is empty\n"
         << "\t\t " << TermColor::BMagenta << "(maxDate)" << TermColor::Reset << "\tin microseconds, the scheduler will stop when the event list is empty, or when the maximum date has been reached\n"
         << "\t\t " << TermColor::BMagenta << "inf" << TermColor::Reset << "\t\tthe simulation will have an infinite duration and can only be stopped when the user presses the 'Q' key" << endl;
    cerr << "\t " << TermColor::BMagenta << "-m <VMpath>:<VMport>" << TermColor::Reset
         << "\tPath to the MeldVM directory and port" << endl;
    cerr << "\t " << TermColor::BMagenta << "-k " << TermColor::Reset
         << "\t\t\tModule type for generic Block Code execution. Options: {BB, RB, SB, C2D, C3D, MR}" << endl;
    cerr << "\t " << TermColor::BMagenta << "-g " << TermColor::Reset
         << "\t\t\tEnable regression testing (export terminal configuration)" << endl;
    cerr << "\t " << TermColor::BMagenta << "-l " << TermColor::Reset
         << "\t\t\tEnable printing of log information to file simulation.log" << endl;
    cerr << "\t " << TermColor::BMagenta << "-i " << TermColor::Reset
         << "\t\t\tEnable printing more detailed simulation stats" << endl;
    cerr << "\t " << TermColor::BMagenta << "-a <seed>" << TermColor::Reset
         << "\t\tSet simulation seed" << endl;
    cerr << "\t " << TermColor::BMagenta << "-e " << TermColor::Reset << "\t\t\tExport configuration when simulation finishes" << endl;
    cerr << "\t " << TermColor::BMagenta << "-h " << TermColor::Reset << "\t\t\tHelp" << endl;
}

CommandLine::CommandLine(int argc, char *argv[], BlockCodeBuilder bcb) {
    read(argc,argv, bcb);
}

void CommandLine::read(int argc, char *argv[], BlockCodeBuilder bcb) {
    try {
        /* Reading the command line */
        argv++;
        argc--;
        while ( (argc > 0) && (argv[0][0] == '-')) {
            switch(argv[0][1]) {
                case 'p':   {
                    //if (programPath != "")
                    //   help();

                    if (argc < 1) {
                        throw CLIParsingError("No meld program provided after -p");
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
                        throw CLIParsingError("MeldVM port must be a number!");
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
                    else throw CLIParsingError("-r and -R options cannot be enabled at the same time");

                } break;

                case 'R': {
                    if (schedulerMode == CMD_LINE_UNDEFINED)
                        schedulerMode = SCHEDULER_MODE_FASTEST;
                    else throw CLIParsingError("-r and -R options cannot be enabled at the same time");
                } break;

                case 'e': {
                    Simulator::exportFinalConfiguration = true;
                } break;

                case 'x': {
                    schedulerAutoStop = true;
                } break;

                case 'c': {
                    // Configuration file, already managed in Simulator constructor
                    if (argc < 2)
                        throw CLIParsingError("No configuration file provided after -c option");

                    configFile = argv[1];
                    Simulator::configFileName = string(configFile);
                    argc--;
                    argv++;
                } break;

                case 's': {
                    schedulerLength = SCHEDULER_LENGTH_BOUNDED;
                    try {
                        maximumDate = stoull(argv[1]);
                    } catch(std::invalid_argument&) {
                        if (strcmp(argv[1], "inf") == 0) {
                            schedulerLength = SCHEDULER_LENGTH_INFINITE;
                        } else {
                            stringstream err;
                            err << "Found unknown parameter after option -s: "
                                << argv[1] << " (Expected <MaximumDate> or \"inf\")" << endl;
                            throw CLIParsingError(err.str());
                        }
                    } catch(std::out_of_range&) {
                        stringstream err;
                        err << "Maximum Date must be an integer and smaller than (2^63 - 1)! "
                            << "Found maxDate=" << argv[1] << endl;
                        throw CLIParsingError(err.str());
                    }

                    argc--;
                    argv++;
                } break;

                case 't': {
                    GlutContext::GUIisEnabled = false;
                } break;

                case 'f' : {
                    GlutContext::setFullScreenMode(true);
                } break;

                case 'w' : {
                    GlutContext::setShadowsMode(false);
                } break;

                case 'k' : {
                    // Already handled by meld blockCode, nothing to do
                    argc--;
                    argv++;
                } break;

                case 'l' : {
                    log_file.open("simulation.log");
                } break;

                case 'g' : {
                    Simulator::regrTesting = true;
                } break;

                case 'i' : {
                    utils::StatsIndividual::enable = true;
                } break;

                case 'a' : {
                    string str(argv[1]);
                    try {
                        simulationSeed = stoi (str);
                        simulationSeedSet = true;
                    } catch(std::invalid_argument&) {
                        stringstream err;
                        err << "Simulation seed must be an integer. Found seed="
                            << argv[1] << endl;
                        throw CLIParsingError(err.str());
                    } catch(std::out_of_range&) {
                        stringstream err;
                        err << "Simulation seed is out of range. Found seed="
                            << argv[1] << endl;
                        throw CLIParsingError(err.str());
                    }

                    argc--;
                    argv++;
                } break;

                default:
                    // Simulate static virtual function call, through a (actually static)
                    //  class member function
                    // @warning this is a very hacky method as it requires the user blockcode                    //  to check whether or not the constructor's argument is NULL
                    BlockCode *bc = bcb(NULL);
                    bool parsed = bc->parseUserCommandLineArgument(argc, &argv);
                    delete bc;

                    if (not parsed) {
                        stringstream err;
                        err << "Unknown command line argument provided: ("
                            << argv[0] << ")" << endl;
                        throw CLIParsingError(err.str());
                    }
            }

            argc--;
            argv++;
        }
    } catch(CLIParsingError const& e) {
        cerr << e.what() << endl << endl;
        help();
        exit(2);
    }
}

bool CommandLine::randomWorldRequested() const {
    return topology != CMD_LINE_UNDEFINED;
}

ModuleType CommandLine::readModuleType(int argc, char **argv) {
    // Locate -k command line argument
    for (int i = 0; i < argc; i++) {
        if (argv[i][0] == '-' && argv[i][1] == 'k') {
            if (!argv[i+1]) break;

            if (strcmp(argv[i+1], "BB") == 0) return BB;
            else if (strcmp(argv[i+1], "RB") == 0) return RB;
            else if (strcmp(argv[i+1], "SB") == 0) return SB;
            else if (strcmp(argv[i+1], "C2D") == 0) return C2D;
            else if (strcmp(argv[i+1], "C3D") == 0) return C3D;
            else if (strcmp(argv[i+1], "MR") == 0) return MR;
            else {
                stringstream err;
                err << "unknown module type: " << argv[i+1] << endl;
                throw CLIParsingError(err.str());
            }
        }
    }

    // Did not find it
    cerr << "error: module type for generic Block Code execution not provided: -k"
         << " {\"BB\", \"RB\", \"SB\", \"C2D\", \"C3D\", \"MR\"}\t" << endl;
    exit(2);
}
