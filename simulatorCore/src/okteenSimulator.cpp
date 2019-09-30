/*!
 * \file okteenSimulator.cpp
 * \brief okteen Simulator
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include "okteenSimulator.h"
#include <string.h>
#include "trace.h"
#include "utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Okteen {

void OkteenSimulator::help() {
    cerr << "VisibleSim:" << endl;
    cerr << "Okteen" << endl;
    exit(EXIT_SUCCESS);
}

OkteenSimulator::OkteenSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
    : BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "OkteenSimulator constructor" << TermColor::Reset << endl;
#endif
}

OkteenSimulator::~OkteenSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "OkteenSimulator destructor" << TermColor::Reset <<endl;
#endif
}

void OkteenSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    simulator =  new OkteenSimulator(argc, argv, bcb);
    simulator->parseConfiguration(argc, argv);
    simulator->startSimulation();
}

void OkteenSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                      int argc, char *argv[]) {
    world = new OkteenWorld(gridSize, gridScale, argc, argv);

    if (GlutContext::GUIisEnabled)
        world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");

    World::setWorld(world);
}

void OkteenSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                                  const Cell3DPosition &pos, const Color &color, bool master) {

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
    ((OkteenWorld*)world)->addBlock(blockId, bcb, pos, color, orientation, master);
}

} // Okteen namespace
