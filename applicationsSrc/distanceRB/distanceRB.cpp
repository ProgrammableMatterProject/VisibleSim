#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "distanceRBCode.h"
using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
	createSimulator(argc, argv, DistanceRBCode::buildNewBlockCode);
	Scheduler *scheduler = getScheduler();

	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
	BaseSimulator::deleteSimulator();
	return(0);
}
