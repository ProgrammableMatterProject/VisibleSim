#include <iostream>
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksBlockCode.h"
#include "simpleColorCode.h"

using namespace std;
using namespace BlinkyBlocks;

int main(int argc, char **argv) {
	createSimulator(argc, argv, SimpleColorCode::buildNewBlockCode);
	
	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
