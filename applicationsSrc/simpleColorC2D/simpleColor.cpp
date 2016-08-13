#include <iostream>
#include "catoms2DSimulator.h"
#include "catoms2DBlockCode.h"
#include "simpleColorCode.h"

using namespace std;
using namespace Catoms2D;

int main(int argc, char **argv) {
	createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);

	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
