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

enum ModuleType {BB, RB, SB, C2D, C3D, MR};

int main(int argc, char **argv) {

	OUTPUT << "\033[1;33m" << "Starting Meld simulation (main) ..." << "\033[0m" << endl;

	BaseSimulator::Simulator::setType(BaseSimulator::Simulator::MELDINTERPRET);

    ModuleType moduleType = BB;
    switch (moduleType) {
    case BB:
        BlinkyBlocks::createSimulator(argc, argv, MeldBlockCode::buildNewBlockCode);
        break;
    default:
        cerr << "error: NOT YET IMPLEMENTED..." << endl;
        break;
    }
    
    Simulator::getSimulator()->printInfo();
	getScheduler()->printInfo();
	BaseSimulator::getWorld()->printInfo();

	deleteSimulator();

	OUTPUT << "\033[1;33m" << "end (main)" << "\033[0m" << endl;
	return(0);
}
