/**
 * @file   NodeSimulator.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:10:11 2019
 *
 * @brief
 *
 *
 */

#include "nodeSimulator.h"

#include <iostream>
#include <string.h>

#include "trace.h"
#include "utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Node {

void NodeSimulator::help() {
    cerr << "VisibleSim:" << endl;
    cerr << "Node" << endl;
    exit(EXIT_SUCCESS);
}

NodeSimulator::NodeSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
    : BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "NodeSimulator constructor" << TermColor::Reset << endl;
#endif
}

NodeSimulator::~NodeSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "NodeSimulator destructor" << TermColor::Reset <<endl;
#endif
}

void NodeSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    simulator =  new NodeSimulator(argc, argv, bcb);
    simulator->parseConfiguration(argc, argv);
    simulator->startSimulation();
}

void NodeSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                      int argc, char *argv[]) {
    world = new NodeWorld(gridSize, gridScale, argc, argv);

    if (GlutContext::GUIisEnabled)
        world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");

    World::setWorld(world);
}

void NodeSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
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
    ((NodeWorld*)world)->addBlock(blockId, bcb, pos, color, orientation, master);
}

} // Node namespace
