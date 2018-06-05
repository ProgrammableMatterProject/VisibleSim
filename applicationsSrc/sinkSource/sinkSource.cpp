#include <iostream>
#include "smartBlocksSimulator.h"
#include "smartBlocksBlockCode.h"
#include "sinkSourceCode.h"

using namespace std;
using namespace SmartBlocks;

int main(int argc, char **argv) {
	createSimulator(argc, argv, SinkSourceCode::buildNewBlockCode);

	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();
	return(0);
}
