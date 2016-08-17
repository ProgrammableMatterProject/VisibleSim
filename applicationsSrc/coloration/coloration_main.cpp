/*
 * leader_election_main.cpp
 *
 *  Created on: 21 avril 2013
 *      Author: nico
 */

#include <iostream>
#include <cstdlib>
#include "smartBlocksSimulator.h"
#include "smartBlocksBlockCode.h"
#include "coloration_BlockCode.h"

using namespace std;
using namespace SmartBlocks;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Smart Blocks simulation (main) ..." << "\033[0m" << endl;

	srand(1);					/* For determinism in testing, rand() should be replaced by generator from BB
								 * and using the seed argument */
	
	createSimulator(argc, argv, Coloration_BlockCode::buildNewBlockCode);

	getSimulator()->printInfo();
	SmartBlocks::getWorld()->printInfo();

	deleteSimulator();

	cout << "\033[1;33m" << "end (main)" << "\033[0m" << endl;

	return(0);
}
