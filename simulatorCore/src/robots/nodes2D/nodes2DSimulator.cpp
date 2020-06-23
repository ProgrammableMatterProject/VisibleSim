/**
 * @file   Nodes2DSimulator.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:10:11 2019
 *
 * @brief
 *
 *
 */

#include <iostream>
#include <string.h>

#include "../../utils/trace.h"
#include "../../utils/utils.h"
#include "nodes2DSimulator.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Nodes2D {

void Nodes2DSimulator::help() {
    cerr << "VisibleSim:" << endl;
    cerr << "Nodes2D" << endl;
    exit(EXIT_SUCCESS);
}

Nodes2DSimulator::Nodes2DSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
    : BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "Nodes2DSimulator constructor" << TermColor::Reset << endl;
#endif
}

Nodes2DSimulator::~Nodes2DSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "Nodes2DSimulator destructor" << TermColor::Reset <<endl;
#endif
}

void Nodes2DSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    simulator =  new Nodes2DSimulator(argc, argv, bcb);
    simulator->parseConfiguration(argc, argv);
    simulator->startSimulation();
}

void Nodes2DSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                      int argc, char *argv[]) {
    world = new Nodes2DWorld(gridSize, gridScale, argc, argv);

    if (GlutContext::GUIisEnabled)
        world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");

    World::setWorld(world);
}

void Nodes2DSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
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
    ((Nodes2DWorld*)world)->addBlock(blockId, bcb, pos, color, orientation, master);
    world->getBlockById(blockId)->blockCode->parseUserBlockElements(blockElt);
}

} // Nodes2D namespace
