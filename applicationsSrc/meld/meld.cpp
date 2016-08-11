#include <iostream>
#include <trace.h>

#include "meldBlockCode.h"

#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "smartBlocksSimulator.h"
#include "smartBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksBlockCode.h"
#include "catoms2DSimulator.h"
#include "catoms2DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"

#include "meldInterpretVM.h"
#include "configStat.h"

using namespace std;

ModuleType readModuleType(int argc, char **argv) {
    // Locate -k command line argument
    for (int i = 0; i < argc; i++) {
        if (argv[i][0] == '-' && argv[i][1] == 'k') {
            if (!argv[i+1]) break;
            
            if (strcmp(argv[i+1], "BB") == 0) return BB;
            else if (strcmp(argv[i+1], "RB") == 0) return RB;
            else if (strcmp(argv[i+1], "SB") == 0) return SB;
            else if (strcmp(argv[i+1], "C2D") == 0) return C2D;
            else if (strcmp(argv[i+1], "C3D") == 0) return C3D;
            else {
                cerr << "error: unknown module type: " << argv[i+1] << endl;
                exit(EXIT_FAILURE);
            }
        }
    }

    // Did not find it
    cerr << "error: module type for meld execution not provided: -k"
         << " {\"BB\", \"RB\", \"SB\", \"C2D\", \"C3D\"}\t" << endl;
    exit(EXIT_FAILURE);   
}

int main(int argc, char **argv) {

	OUTPUT << "\033[1;33m" << "Starting Meld simulation (main) ..." << "\033[0m" << endl;

	BaseSimulator::Simulator::setType(BaseSimulator::Simulator::MELDINTERPRET);

    MeldBlockCode::moduleType = readModuleType(argc, argv);
    switch (MeldBlockCode::moduleType) {
    case BB:
        BlinkyBlocks::createSimulator(argc, argv, MeldBlockCode::buildNewBlockCode);
        break;
    case RB:
        RobotBlocks::createSimulator(argc, argv, MeldBlockCode::buildNewBlockCode);
        break;
    case SB:
        SmartBlocks::createSimulator(argc, argv, MeldBlockCode::buildNewBlockCode);
        break;
    case C2D:
        Catoms2D::createSimulator(argc, argv, MeldBlockCode::buildNewBlockCode);
        break;
    case C3D:
        Catoms3D::createSimulator(argc, argv, MeldBlockCode::buildNewBlockCode);
        break;

    default:
        cerr << "error: generic Meld for this module is not yet implemented..." << endl;
        break;
    }
    
    Simulator::getSimulator()->printInfo();

	deleteSimulator();

	OUTPUT << "\033[1;33m" << "end (main)" << "\033[0m" << endl;
	return(0);
}
