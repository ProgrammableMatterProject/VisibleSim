#include <iostream>
#include "datomsSimulator.h"
#include "datomsBlockCode.h"
#include "datomsRotateCode.h"

using namespace std;
using namespace Datoms;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catom3D simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, DatomsRotateCode::buildNewBlockCode);
	Scheduler *scheduler = getScheduler();

	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
