#include <iostream>
#include "smartBlocksSimulator.h"
#include "smartBlocksBlockCode.h"
#include "simpleColorCode.h"

using namespace std;
using namespace SmartBlocks;

int main(int argc, char **argv) {
	createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);
	Scheduler *scheduler = getScheduler();

	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
