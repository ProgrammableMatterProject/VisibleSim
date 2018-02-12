/*
 * catoms3DSimulator.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include <string.h>
#include "trace.h"
#include "utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Catoms3D {

void Catoms3DSimulator::help() {
	cerr << "VisibleSim:" << endl;
	cerr << "Catoms3D" << endl;
	exit(EXIT_SUCCESS);
}

Catoms3DSimulator::Catoms3DSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
	: BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
	OUTPUT << "\033[1;34m" << "Catoms3DSimulator constructor" << "\033[0m" << endl;
#endif
}

Catoms3DSimulator::~Catoms3DSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE    
	OUTPUT << "\033[1;34m" << "Catoms3DSimulator destructor" << "\033[0m" <<endl;
#endif
}

void Catoms3DSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
	simulator =  new Catoms3DSimulator(argc, argv, bcb);
	simulator->parseConfiguration(argc, argv);
	simulator->startSimulation();
}

void Catoms3DSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                  int argc, char *argv[]) {
    world = new Catoms3DWorld(gridSize, gridScale, argc, argv);

	if (GlutContext::GUIisEnabled) 
		world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");
	
    World::setWorld(world);
}

void Catoms3DSimulator::loadBlock(TiXmlElement *blockElt, bID blockId,
                                  BlockCodeBuilder bcb, const Cell3DPosition &pos,
                                  const Color &color, bool master) {

	// Any additional configuration file parsing exclusive to this type of block should be performed
	//  here, using the blockElt TiXmlElement.

	// set the orientation
	int orientation = 0;
	const char *attr = blockElt->Attribute("orientation");
	if (attr) {
		orientation = atoi(attr);
#ifdef DEBUG_WORLD_LOAD
        OUTPUT << "orientation : " << orientation << endl;
#endif
	}

	// Finally, add block to the world
	((Catoms3DWorld*)world)->addBlock(blockId, bcb, pos, color, orientation, master);
}

} // Catoms3D namespace
