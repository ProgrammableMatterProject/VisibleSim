/*
 * smartblock01.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include "catoms2DSimulator.h"
#include "catoms2DBlockCode.h"
#include "noneCatoms2DBlockCode.h"

using namespace std;
using namespace Catoms2D;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catoms2D simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, NoneCatoms2DBlockCode::buildNewBlockCode);
	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
/*
	scheduler->start(SCHEDULER_MODE_FASTEST);

	scheduler->waitForSchedulerEnd();
*/
	deleteSimulator();

	cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

	return(0);
}
