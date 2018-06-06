/*
 * reconfCatoms3D.cpp
 *
 *  Created on: 17 October 2016
 *  Author: Thadeu
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "reconfCatoms3DBlockCode.h"

using namespace std;
using namespace Catoms3D;

int main(int argc, char **argv) {
	cout << "\033[1;33m" << "Starting Catoms3D simulation (main) ..." << "\033[0m" << endl;

	createSimulator(argc, argv, ReconfCatoms3DBlockCode::buildNewBlockCode);
	getSimulator()->printInfo();
	BaseSimulator::getWorld()->printInfo();

	deleteSimulator();
	return(0);
}

void info()
{
    cout << "Number of messages to get initial info: " << NeighborMessages::nMessagesGetInfo << endl;
    cout << "Number of messages to get permission to add blocks: " << Neighborhood::numberMessagesToAddBlock << endl;
}
void printXML()
{
    cout << "<blockList color=\"128,128,128\" blocksize=\"10,10,10\">" << endl;
    for (int i = 1; i <= BaseSimulator::getWorld()->getNbBlocks(); i++) {
        Cell3DPosition pos =  BaseSimulator::getWorld()->getBlockById(i)->position;
        cout << "<block position=\"" << pos[0] << "," << pos[1] << "," << pos[2] << "\"/>" << endl;
    }
    cout << "</blockList>" << endl;
}
