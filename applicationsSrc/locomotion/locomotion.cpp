#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "locomotionCode.h"

using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
	createSimulator(argc, argv, LocomotionCode::buildNewBlockCode);
	Scheduler *scheduler = getScheduler();

	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
