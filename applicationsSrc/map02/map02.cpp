/*
 * robot02.cpp
 *
 *  Created on: 30 mars 2015
 *      Author: Vincent
 */

#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "map02BlockCode.h"

using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Mapping simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, Robot02BlockCode::buildNewBlockCode);
	Scheduler *scheduler = getScheduler();
	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();

	/*
	scheduler->start(SCHEDULER_MODE_FASTEST);
	scheduler->waitForSchedulerEnd();
	*/
	
	deleteSimulator();

	return(0);
}
