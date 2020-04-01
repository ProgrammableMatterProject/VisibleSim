/**
 * @file   HexanodeSimulator.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:10:11 2019
 *
 * @brief
 *
 *
 */

#include "hexanodeSimulator.h"

#include <iostream>
#include <string.h>

#include "trace.h"
#include "utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Hexanode {

void HexanodeSimulator::help() {
    cerr << "VisibleSim:" << endl;
    cerr << "Hexanode" << endl;
    exit(EXIT_SUCCESS);
}

HexanodeSimulator::HexanodeSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
    : BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "HexanodeSimulator constructor" << TermColor::Reset << endl;
#endif
}

HexanodeSimulator::~HexanodeSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "HexanodeSimulator destructor" << TermColor::Reset <<endl;
#endif
}

void HexanodeSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    simulator =  new HexanodeSimulator(argc, argv, bcb);
    simulator->parseConfiguration(argc, argv);
    simulator->startSimulation();
}

void HexanodeSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                      int argc, char *argv[]) {
    world = new HexanodeWorld(gridSize, gridScale, argc, argv);

    if (GlutContext::GUIisEnabled)
        world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");

    World::setWorld(world);
}

void HexanodeSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
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
    ((HexanodeWorld*)world)->addBlock(blockId, bcb, pos, color, orientation, master);
}

} // Hexanode namespace
