/*
 * map04.cpp
 *
 *  Created on: may 22 2015
 *      Author: Vincent
 */

#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "map04BlockCode.h"

using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Mapping simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, Map04BlockCode::buildNewBlockCode);
	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();

	/*
	scheduler->start(SCHEDULER_MODE_FASTEST);
	scheduler->waitForSchedulerEnd();
	*/
	
	deleteSimulator();

	return(0);
}
