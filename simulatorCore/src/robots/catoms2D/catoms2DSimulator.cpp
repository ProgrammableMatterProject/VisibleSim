/*
 * catoms2DSimulator.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include <string.h>

#include "catoms2DSimulator.h"
#include "../../utils/trace.h"
#include "../../utils/utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Catoms2D {

void Catoms2DSimulator::help() {
    cerr << "VisibleSim: " << endl;
    cerr << "Catoms2D" << endl;
    exit(EXIT_SUCCESS);
}

Catoms2DSimulator::Catoms2DSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
    : BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "Catoms2DSimulator constructor" << "" << TermColor::Reset << endl;
#endif
}

Catoms2DSimulator::~Catoms2DSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "Catoms2DSimulator destructor" << "" << TermColor::Reset <<endl;
#endif
}

void Catoms2DSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    simulator =  new Catoms2DSimulator(argc, argv, bcb);
    simulator->parseConfiguration(argc, argv);
    simulator->startSimulation();
}

void Catoms2DSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                  int argc, char *argv[]) {
    world = new Catoms2DWorld(gridSize, gridScale, argc,argv);

#ifdef WIN32
    string directory = string(ROOT_DIR) + "/simulatorCore/resources/textures/latticeTextures";
#else
    string directory = "../../simulatorCore/resources/textures/latticeTextures";
#endif
    if (GlutContext::GUIisEnabled)
        world->loadTextures(directory);

    World::setWorld(world);
}

void Catoms2DSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                                  const Cell3DPosition &pos, const Color &color, uint8_t orient) {

    // Any additional configuration file parsing exclusive to this type of block should be performed
    //  here, using the blockElt TiXmlElement.

    // @todo: parse angle orientation

    // Finally, add block to the world
    ((Catoms2DWorld*)world)->addBlock(blockId, bcb, pos, color, 0);
    world->getBlockById(blockId)->blockCode->parseUserBlockElements(blockElt);
}

} // catoms2D namespace
