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
#include "commandLine.h"
#include "configStat.h"

using namespace std;

int main(int argc, char **argv) {

	OUTPUT << "\033[1;33m" << "Starting Meld simulation (main) ..." << "\033[0m" << endl;

	BaseSimulator::Simulator::setType(BaseSimulator::Simulator::MELDINTERPRET);

    MeldBlockCode::moduleType = CommandLine::readModuleType(argc, argv);
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
