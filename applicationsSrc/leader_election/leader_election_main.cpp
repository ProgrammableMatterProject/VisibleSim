/*
 * leader_election_main.cpp
 *
 *  Created on: 21 avril 2013
 *      Author: nico
 */

#include <iostream>
#include <cstdlib>
#include <time.h>
#include "smartBlocksSimulator.h"
#include "smartBlocksBlockCode.h"
#include "leader_election_BlockCode.h"

using namespace std;
using namespace SmartBlocks;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Smart Blocks simulation (main) ..." << "\033[0m" << endl;

	srand( time(NULL));

	createSimulator(argc, argv, Leader_election_BlockCode::buildNewBlockCode);
	SmartBlocksScheduler *scheduler = SmartBlocks::getScheduler();


	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();

	scheduler->start(SCHEDULER_MODE_FASTEST);

	scheduler->waitForSchedulerEnd();

	deleteSimulator();

	cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

	return(0);
}
