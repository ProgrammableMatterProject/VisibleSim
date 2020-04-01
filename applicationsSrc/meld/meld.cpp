#include <iostream>

#include "utils/trace.h"
#include "meldBlockCode.h"

#include "robots/slidingCubes/slidingCubesSimulator.h"
#include "robots/slidingCubes/slidingCubesBlockCode.h"
#include "robots/smartBlocks/smartBlocksSimulator.h"
#include "robots/smartBlocks/smartBlocksBlockCode.h"
#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"
#include "robots/catoms2D/catoms2DSimulator.h"
#include "robots/catoms2D/catoms2DBlockCode.h"
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"

#include "meld/meldInterpretVM.h"
#include "utils/commandLine.h"
#include "stats/configStat.h"

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
        SlidingCubes::createSimulator(argc, argv, MeldBlockCode::buildNewBlockCode);
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
