/*
 * reconfCatoms3D.cpp
 *
 *  Created on: 17 October 2016
 *  Author: Thadeu
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "reconfCatoms3DBlockCode.h"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catoms3D simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, ReconfCatoms3DBlockCode::buildNewBlockCode);
	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();

	deleteSimulator();
	return(0);
}
