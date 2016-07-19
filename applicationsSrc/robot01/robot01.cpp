/*
 * robot01.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"
#include "robot01BlockCode.h"

using namespace std;
using namespace RobotBlocks;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting RobotBlocks simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, Robot01BlockCode::buildNewBlockCode);
    Scheduler *scheduler = getScheduler();
	BaseSimulator::getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
/*
	scheduler->start(SCHEDULER_MODE_FASTEST);

	scheduler->waitForSchedulerEnd();
*/
	deleteSimulator();

	return(0);
}
