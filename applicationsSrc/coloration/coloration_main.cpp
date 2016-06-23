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
#include "coloration_BlockCode.h"

using namespace std;
using namespace SmartBlocks;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Smart Blocks simulation (main) ..." << "\033[0m" << endl;

	srand( time(NULL));

	createSimulator(argc, argv, Coloration_BlockCode::buildNewBlockCode);

	getSimulator()->printInfo();
	getScheduler()->printInfo();
	SmartBlocks::getWorld()->printInfo();

	getScheduler()->start(SCHEDULER_MODE_FASTEST);

	getScheduler()->waitForSchedulerEnd();

	deleteSimulator();

	cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

	return(0);
}
