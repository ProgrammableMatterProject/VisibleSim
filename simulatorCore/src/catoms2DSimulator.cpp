/*
 * catoms2DSimulator.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include <string.h>

#include "catoms2DSimulator.h"
#include "trace.h"
#include "utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Catoms2D {

Catoms2DBlockCode*(* Catoms2DSimulator::buildNewBlockCode)(Catoms2DBlock*)=NULL;

void Catoms2DSimulator::help() {
	cerr << "VisibleSim: " << endl;
	cerr << "Catoms2D" << endl;
	exit(EXIT_SUCCESS);
}

Catoms2DSimulator::Catoms2DSimulator(int argc, char *argv[],
									 Catoms2DBlockCode *(*catoms2DBlockCodeBuildingFunction)
									 (Catoms2DBlock*)) : BaseSimulator::Simulator(argc, argv) {

	OUTPUT << "\033[1;34m" << "Catoms2DSimulator constructor" << "\033[0m" << endl;

	testMode = false;

	// PTHY: Note: function pointer cast to generic type, safe according to specifications as it will be used
	//  only after reconversion
	buildNewBlockCode = catoms2DBlockCodeBuildingFunction;
	newBlockCode = (BlockCode *(*)(BuildingBlock *))catoms2DBlockCodeBuildingFunction;
	parseWorld(argc, argv);
	parseBlockList();
	parseTarget(2);

	((Catoms2DWorld*)world)->linkBlocks();

//	getScheduler()->sem_schedulerStart->post();
//	getScheduler()->setState(Scheduler::NOTSTARTED);

	if (!testMode) {
		GlutContext::mainLoop();
	}

}

Catoms2DSimulator::~Catoms2DSimulator() {
	OUTPUT << "\033[1;34m" << "Catoms2DSimulator destructor" << "\033[0m" <<endl;
}

void Catoms2DSimulator::createSimulator(int argc, char *argv[],
										Catoms2DBlockCode *(*catoms2DBlockCodeBuildingFunction)
										(Catoms2DBlock*)) {
	simulator =  new Catoms2DSimulator(argc, argv, catoms2DBlockCodeBuildingFunction);
}

void Catoms2DSimulator::deleteSimulator() {
	delete((Catoms2DSimulator*)simulator);
}

void Catoms2DSimulator::loadWorld(int lx, int ly, int lz, int argc, char *argv[]) {
	Catoms2DWorld::createWorld(lx,ly,lz,argc,argv);
	world = Catoms2DWorld::getWorld();
	world->loadTextures("../../simulatorCore/catoms2DTextures");
}

void Catoms2DSimulator::loadScheduler() {
	Catoms2DScheduler::createScheduler();
	scheduler = Catoms2DScheduler::getScheduler();
}

void Catoms2DSimulator::loadBlock(TiXmlElement *blockElt, int blockId,
								  BlockCode *(*buildingBlockCodeBuildingFunction)(BuildingBlock*),
								  const Cell3DPosition &pos, const Color &color, bool master) {

	// Any additional configuration file parsing exclusive to this type of block should be performed
	//  here, using the blockElt TiXmlElement.

	// ...Parsing code...

	// Finally, add block to the world
	((Catoms2DWorld*)world)->addBlock(blockId,
									  (Catoms2DBlockCode *(*)(Catoms2DBlock *))
									  buildingBlockCodeBuildingFunction,
									  pos, color, master);
}

void Catoms2DSimulator::loadTargetAndCapabilities(vector<Cell3DPosition> targetCells) {

	// Add target cells to world
	((Catoms2DWorld*)world)->initTargetGrid();
	for (Cell3DPosition p : targetCells) {
		((Catoms2DWorld*)world)->setTargetGrid(fullCell, p[0], p[1], p[2]);
	}

	// then parse and load capabilities...
	TiXmlNode *nodeCapa = xmlWorldNode->FirstChild("capabilities");
	if (nodeCapa) {
		((Catoms2DWorld*)world)->setCapabilities(new Catoms2DCapabilities(nodeCapa));
	}
}

} // catoms2D namespace
