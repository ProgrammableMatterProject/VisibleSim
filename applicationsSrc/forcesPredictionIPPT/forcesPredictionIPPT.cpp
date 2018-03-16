#include <iostream>
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksBlockCode.h"
#include "forcesPredictionIPPTCode.h"

using namespace std;
using namespace BlinkyBlocks;

int main(int argc, char **argv) {
	createSimulator(argc, argv, ForcesPredictionIPPTCode::buildNewBlockCode);
	Scheduler *scheduler = getScheduler();

	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
