/*
 * map03.cpp
 *
 *  Created on: 30 avril 2015
 *      Author: Vincent
 */

#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "map03BlockCode.h"

using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Mapping simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, Map03BlockCode::buildNewBlockCode);
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
