#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "simpleColorCode.h"

using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
	createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);
	RobotBlocksScheduler *scheduler = RobotBlocks::getScheduler();

	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
