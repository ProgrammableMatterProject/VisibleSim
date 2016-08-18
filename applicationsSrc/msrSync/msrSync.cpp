#include <iostream>
#include <trace.h>

#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksBlockCode.h"
#include "msrSyncBlockCode.h"


using namespace std;
using namespace BlinkyBlocks;

int main(int argc, char **argv) {
	
	OUTPUT << "\033[1;33m" << "Starting Blinky Blocks simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, msrSyncBlockCode::buildNewBlockCode);
	
	{
		using namespace BaseSimulator;

		Simulator *s = Simulator::getSimulator();
		s->printInfo();
	}

	Simulator::getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
		
	deleteSimulator();
	
	OUTPUT << "\033[1;33m" << "end (main)" << "\033[0m" << endl;
	return(0);
}
