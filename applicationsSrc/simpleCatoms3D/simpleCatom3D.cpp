/*
 * smartblock01.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "simpleCatom3DBlockCode.h"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, SimpleCatom3DBlockCode::buildNewBlockCode);
	Catoms3DScheduler *scheduler = Catoms3D::getScheduler();
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
