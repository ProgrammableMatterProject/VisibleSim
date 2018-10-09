/*
 * robotBlocksSimulator.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include <string.h>

#include "robotBlocksSimulator.h"
#include "trace.h"

using namespace std;

namespace RobotBlocks {

void RobotBlocksSimulator::help() {
	cerr << "VisibleSim:" << endl;
	cerr << "Robot01" << endl;
	exit(EXIT_SUCCESS);
}

RobotBlocksSimulator::RobotBlocksSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
	: BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
	OUTPUT << "\033[1;34m" << "RobotBlocksSimulator constructor" << "\033[0m" << endl;
#endif
}

RobotBlocksSimulator::~RobotBlocksSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE    
	OUTPUT << "\033[1;34m" << "RobotBlocksSimulator destructor" << "\033[0m" <<endl;
#endif
}

void RobotBlocksSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
	simulator =  new RobotBlocksSimulator(argc, argv, bcb);
	simulator->parseConfiguration(argc, argv);
	simulator->startSimulation();
}

void RobotBlocksSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
									int argc, char *argv[]) {
    world = new RobotBlocksWorld(gridSize, gridScale, argc,argv);

    if (GlutContext::GUIisEnabled)
		world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");
	
    World::setWorld(world);
}

void RobotBlocksSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
									 const Cell3DPosition &pos, const Color &color, bool master) {

	// Any additional configuration file parsing exclusive to this type of block should be performed
	//  here, using the blockElt TiXmlElement.

	// ...Parsing code...

	// Finally, add block to the world
	((RobotBlocksWorld*)world)->addBlock(blockId, bcb, pos, color, 0, master);
}

} // RobotBlocks namespace
