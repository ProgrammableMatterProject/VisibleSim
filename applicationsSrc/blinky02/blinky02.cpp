/*
 * blinky02.cpp
 *
 *  Created on: 06 juin 2013
 *      Author: ben
 */

#include <iostream>
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksBlockCode.h"
#include "blinky02BlockCode.h"

using namespace std;
using namespace BlinkyBlocks;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting BlinkyBlocks simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, Blinky02BlockCode::buildNewBlockCode);
	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();
	deleteSimulator();

	cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

	return(0);
}
