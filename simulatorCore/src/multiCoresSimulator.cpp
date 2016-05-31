/*
 * multiCoresSimulator.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "multiCoresSimulator.h"

using namespace std;

namespace MultiCores {

MultiCoresBlockCode*(* MultiCoresSimulator::buildNewBlockCode)(MultiCoresBlock*)=NULL;

MultiCoresSimulator::MultiCoresSimulator(int argc, char *argv[], MultiCoresBlockCode *(*multiCoreBlockCodeBuildingFunction)(MultiCoresBlock*)) : BaseSimulator::Simulator(argc, argv) {
	cout << "\033[1;34m" << "MultiCoresSimulator constructor" << "\033[0m" << endl;

	buildNewBlockCode = multiCoreBlockCodeBuildingFunction;

	createScheduler();
	createWorld();
}

MultiCoresSimulator::~MultiCoresSimulator() {
	cout << "\033[1;34m" << "MultiCoresSimulator destructor" << "\033[0m" <<endl;
}

void MultiCoresSimulator::createSimulator(int argc, char *argv[], MultiCoresBlockCode *(*multiCoreBlockCodeBuildingFunction)(MultiCoresBlock*)) {
	simulator =  new MultiCoresSimulator(argc, argv, multiCoreBlockCodeBuildingFunction);
}

void MultiCoresSimulator::deleteSimulator() {
	delete((MultiCoresSimulator*)simulator);
}

} // MultiCores namespace
