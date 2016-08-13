#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "simpleColorCode.h"

using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
	createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);

	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
