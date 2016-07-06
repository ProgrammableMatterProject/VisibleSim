#include <iostream>
#include <trace.h>

#include "C3DMeldBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "meldInterpretVM.h"
#include "configStat.h"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {

	OUTPUT << "\033[1;33m" << "Starting Catoms3D simulation (main) ..." << "\033[0m" << endl;

	BaseSimulator::Simulator::setType(BaseSimulator::Simulator::MELDINTERPRET);
	createSimulator(argc, argv, C3DMeldBlockCode::buildNewBlockCode);

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
