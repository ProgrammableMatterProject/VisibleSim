/*
 * flavio01.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */


#include <iostream>
#include "multiCoresSimulator.h"
#include "network.h"
#include "flavio01BlockCode.h"

using namespace std;
using namespace MultiCores;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting MultiCores simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, Flavio01BlockCode::buildNewBlockCode);

	{
		using namespace BaseSimulator;

		Simulator *s = Simulator::getSimulator();
		s->printInfo();
	}
/*
	Message m;
	cout << m.getMessageName() << endl;
*/
	getSimulator()->printInfo();
	getScheduler()->printInfo();
	getWorld()->printInfo();

	getWorld()->addBlock(-1,Flavio01BlockCode::buildNewBlockCode,0,0,0,1,1,1);
	getWorld()->addBlock(-1,Flavio01BlockCode::buildNewBlockCode,0,0,0,1,1,1);

	getScheduler()->waitForSchedulerEnd();

	deleteSimulator();

	cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

	return(0);
}
