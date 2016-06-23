/*
 * robotBlocksSimulator.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "robotBlocksSimulator.h"
#include <string.h>
#include "trace.h"

using namespace std;

namespace RobotBlocks {

RobotBlocksBlockCode*(* RobotBlocksSimulator::buildNewBlockCode)(RobotBlocksBlock*)=NULL;

void RobotBlocksSimulator::help() {
	cerr << "VisibleSim:" << endl;
	cerr << "Robot01" << endl;
	exit(EXIT_SUCCESS);
}

RobotBlocksSimulator::RobotBlocksSimulator(int argc, char *argv[],
										   RobotBlocksBlockCode *(*robotBlocksBlockCodeBuildingFunction)
										   (RobotBlocksBlock*)) : BaseSimulator::Simulator(argc, argv) {
	OUTPUT << "\033[1;34m" << "RobotBlocksSimulator constructor" << "\033[0m" << endl;

	testMode = false;

	buildNewBlockCode = robotBlocksBlockCodeBuildingFunction;
	newBlockCode = (BlockCode *(*)(BuildingBlock *))robotBlocksBlockCodeBuildingFunction;
	parseWorld(argc, argv);

	((RobotBlocksWorld*)world)->linkBlocks();

//	getScheduler()->sem_schedulerStart->post();
//	getScheduler()->setState(Scheduler::NOTSTARTED);

	if (!testMode) {
		GlutContext::mainLoop();
	}
}

RobotBlocksSimulator::~RobotBlocksSimulator() {
	OUTPUT << "\033[1;34m" << "RobotBlocksSimulator destructor" << "\033[0m" <<endl;
}

void RobotBlocksSimulator::createSimulator(int argc, char *argv[], RobotBlocksBlockCode *(*robotBlocksBlockCodeBuildingFunction)(RobotBlocksBlock*)) {
	simulator =  new RobotBlocksSimulator(argc, argv, robotBlocksBlockCodeBuildingFunction);
}

void RobotBlocksSimulator::deleteSimulator() {
	delete((RobotBlocksSimulator*)simulator);
}

void RobotBlocksSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
									int argc, char *argv[]) {
    world = new RobotBlocksWorld(gridSize, gridScale, argc,argv);
    world->loadTextures("../../simulatorCore/robotBlocksTextures");
    World::setWorld(world);
}

void RobotBlocksSimulator::loadBlock(TiXmlElement *blockElt, int blockId,
									 BlockCode *(*buildingBlockCodeBuildingFunction)(BuildingBlock*),
									 const Cell3DPosition &pos, const Color &color, bool master) {

	// Any additional configuration file parsing exclusive to this type of block should be performed
	//  here, using the blockElt TiXmlElement.

	// ...Parsing code...

	// Finally, add block to the world
	((RobotBlocksWorld*)world)->addBlock(blockId,
										 (RobotBlocksBlockCode *(*)(RobotBlocksBlock *))
										 buildingBlockCodeBuildingFunction,
										 pos, color, master);
}

void RobotBlocksSimulator::loadTargetAndCapabilities(vector<Cell3DPosition> targetCells) {

	// Add target cells to world
	((RobotBlocksWorld*)world)->initTargetGrid();
	for (Cell3DPosition p : targetCells) {
		((RobotBlocksWorld*)world)->setTargetGrid(fullCell, p[0], p[1], p[2]);
	}

	// then parse and load capabilities...
	TiXmlNode *nodeCapa = xmlWorldNode->FirstChild("capabilities");
	if (nodeCapa) {
		((RobotBlocksWorld*)world)->setCapabilities(new RobotBlocksCapabilities(nodeCapa));
	}
}


} // RobotBlocks namespace
