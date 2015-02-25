/*
 * smartblock01.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include "smartBlocksSimulator.h"
#include "smartBlocksBlockCode.h"
#include "smartblock1BlockCode.h"

using namespace std;
using namespace SmartBlocks;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Smart Blocks simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, SmartBlock1BlockCode::buildNewBlockCode);
	SmartBlocksScheduler *scheduler = SmartBlocks::getScheduler();
	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
/*
	scheduler->start(SCHEDULER_MODE_FASTEST);

	scheduler->waitForSchedulerEnd();
*/
	deleteSimulator();

	cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

	return(0);
}
