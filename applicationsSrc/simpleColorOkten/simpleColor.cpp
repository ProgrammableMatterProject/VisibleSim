#include <iostream>
#include "oktenSimulator.h"
#include "oktenBlockCode.h"
#include "simpleColorCode.h"

using namespace std;
using namespace Okten;

int main(int argc, char **argv) {
	createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);

	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
