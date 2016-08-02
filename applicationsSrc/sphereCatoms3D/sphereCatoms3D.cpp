/*
 * csgCatoms3D.cpp
 *
 *  Created on: 06 August 2015
 *  Author: Thadeu
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "sphereCatoms3DBlockCode.h"
#include <ctime>

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, SphereCatoms3DBlockCode::buildNewBlockCode);
	Scheduler *scheduler = getScheduler();
	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
/*
	scheduler->start(SCHEDULER_MODE_FASTEST);
	scheduler->waitForSchedulerEnd();
*/
	deleteSimulator();

	cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;
    cout << "Count radius 1 = " << SphereCatoms3DBlockCode::radius[1] << endl;
    cout << "Count radius 2 = " << SphereCatoms3DBlockCode::radius[2] << endl;
    cout << "Count radius 3 = " << SphereCatoms3DBlockCode::radius[3] << endl;
    cout << "Count radius 3 = " << SphereCatoms3DBlockCode::radius[4] << endl;

	return(0);
}
