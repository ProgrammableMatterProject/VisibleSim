#include <iostream>
#include "okteenSimulator.h"
#include "okteenBlockCode.h"
#include "simpleColorCode.h"

using namespace std;
using namespace Okteen;

int main(int argc, char **argv) {
	createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);

	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
