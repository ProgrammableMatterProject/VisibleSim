#include <iostream>
#include <trace.h>

#include "C2DMeldBlockCode.h"
#include "catoms2DSimulator.h"
#include "catoms2DBlockCode.h"
#include "meldInterpretVM.h"
#include "configStat.h"

using namespace std;
using namespace Catoms2D;

int main(int argc, char **argv) {

	OUTPUT << "\033[1;33m" << "Starting Catoms2D simulation (main) ..." << "\033[0m" << endl;

	BaseSimulator::Simulator::setType(BaseSimulator::Simulator::MELDINTERPRET);
	createSimulator(argc, argv, C2DMeldBlockCode::buildNewBlockCode);

	{
		using namespace BaseSimulator;

		Simulator *s = Simulator::getSimulator();
		s->printInfo();
	}

	getSimulator()->printInfo();
	getScheduler()->printInfo();
	BaseSimulator::getWorld()->printInfo();

	// getScheduler()->waitForSchedulerEnd();

	deleteSimulator();

	OUTPUT << "\033[1;33m" << "end (main)" << "\033[0m" << endl;
	return(0);
}
