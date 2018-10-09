#include <iostream>
#include "simpleMotionCode.h"

using namespace std;
using namespace Okteen;

int main(int argc, char **argv) {
	createSimulator(argc, argv, SimpleMotionCode::buildNewBlockCode);
	Scheduler *scheduler = getScheduler();

	getSimulator()->printInfo();
	scheduler->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
