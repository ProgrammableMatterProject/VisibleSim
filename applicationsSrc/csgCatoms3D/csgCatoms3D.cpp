/*
 * csgCatoms3D.cpp
 *
 *  Created on: 06 August 2015
 *  Author: Thadeu
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "csgCatoms3DBlockCode.h"

using namespace std;
using namespace Catoms3D;

int difference = 0;
int total = 0;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, CsgCatoms3DBlockCode::buildNewBlockCode);
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
    cout << "Difference = " << difference << endl;
    cout << "Total = " << total << endl;

	return(0);
}
