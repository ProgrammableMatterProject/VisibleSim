#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "distanceRBCode.h"
using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
	createSimulator(argc, argv, DistanceRBCode::buildNewBlockCode);

	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
	BaseSimulator::deleteSimulator();
	return(0);
}
