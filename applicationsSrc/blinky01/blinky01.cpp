/*
 * blinky01.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksBlockCode.h"
#include "blinky01BlockCode.h"
#include "meldProcessDebugger.h"
#include "meldProcessVM.h"
#include "configStat.h"
#include <trace.h> 

using namespace std;
using namespace BlinkyBlocks;

int main(int argc, char **argv) {
	
	OUTPUT << "\033[1;33m" << "Starting Blinky Blocks simulation (main) ..." << "\033[0m" << endl;

	BaseSimulator::Simulator::setType(BaseSimulator::Simulator::MELDPROCESS);
	createSimulator(argc, argv, Blinky01BlockCode::buildNewBlockCode);

	//ConfigStat stat(BaseSimulator::getWorld());
	//stat.print();
	
	/*
	{
		using namespace BaseSimulator;

		Simulator *s = Simulator::getSimulator();
		s->printInfo();
	}
	* getSimulator()->printInfo();
	* BlinkyBlocks::getScheduler()->printInfo();
	* BaseSimulator::getWorld()->printInfo();
	*/
		
	BlinkyBlocks::getScheduler()->waitForSchedulerEnd();
		
	if (MeldProcess::MeldProcessVM::isInDebuggingMode()) {
		MeldProcess::getDebugger()->waitForDebuggerEnd();
	}
	
	deleteSimulator();
	
	OUTPUT << "\033[1;33m" << "end (main)" << "\033[0m" << endl;
	return(0);
}
