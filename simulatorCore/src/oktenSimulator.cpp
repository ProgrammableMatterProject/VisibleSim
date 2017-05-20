/*!
 * \file oktenSimulator.cpp
 * \brief okten Simulator
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include "oktenSimulator.h"
#include <string.h>
#include "trace.h"
#include "utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Okten {

void OktenSimulator::help() {
	cerr << "VisibleSim:" << endl;
	cerr << "Okten" << endl;
	exit(EXIT_SUCCESS);
}

OktenSimulator::OktenSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
	: BaseSimulator::Simulator(argc, argv, bcb) {
	OUTPUT << "\033[1;34m" << "OktenSimulator constructor" << "\033[0m" << endl;
}

OktenSimulator::~OktenSimulator() {
	OUTPUT << "\033[1;34m" << "OktenSimulator destructor" << "\033[0m" <<endl;
}

void OktenSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
	simulator =  new OktenSimulator(argc, argv, bcb);
	simulator->parseConfiguration(argc, argv);
	simulator->startSimulation();
}

void OktenSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
				      int argc, char *argv[]) {
    world = new OktenWorld(gridSize, gridScale, argc, argv);

	if (GlutContext::GUIisEnabled)
		world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");

    World::setWorld(world);
}

void OktenSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
								  const Cell3DPosition &pos, const Color &color, bool master) {

	// Any additional configuration file parsing exclusive to this type of block should be performed
	//  here, using the blockElt TiXmlElement.

	// set the orientation
	int orientation = 0;
	const char *attr = blockElt->Attribute("orientation");
	if (attr) {
		orientation = atoi(attr);
		OUTPUT << "orientation : " << orientation << endl;
	}

	// Finally, add block to the world
	((OktenWorld*)world)->addBlock(blockId, bcb, pos, color, orientation, master);
}

} // Okten namespace
