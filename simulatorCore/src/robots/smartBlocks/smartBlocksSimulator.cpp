/*
 * smartBlocksSimulator.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>

#include "smartBlocksSimulator.h"

using namespace std;

namespace SmartBlocks {

SmartBlocksSimulator::SmartBlocksSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
    : BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    cout << TermColor::LifecycleColor << "SmartBlocksSimulator constructor" << TermColor::Reset << endl;
#endif
}

SmartBlocksSimulator::~SmartBlocksSimulator() {
    //MODIF NICO : deleteScheduler() est déjà appellée par BaseSimulator::~Simulator()
    //~ deleteScheduler();
#ifdef DEBUG_OBJECT_LIFECYCLE
    cout << TermColor::LifecycleColor << "SmartBlocksSimulator destructor" << TermColor::Reset <<endl;
#endif
}

void SmartBlocksSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    simulator =  new SmartBlocksSimulator(argc, argv, bcb);
    simulator->parseConfiguration(argc, argv);
    simulator->startSimulation();
}

void SmartBlocksSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                     int argc, char *argv[]) {
    world = new SmartBlocksWorld(gridSize, gridScale, argc,argv);

    if (GlutContext::GUIisEnabled)
        world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");

    World::setWorld(world);
}

void SmartBlocksSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                                     const Cell3DPosition &pos, const Color &color, bool master) {

    // Any additional configuration file parsing exclusive to this type of block should be performed
    //  here, using the blockElt TiXmlElement.

    // ...Parsing code...

    // Finally, add block to the world
    ((SmartBlocksWorld*)world)->addBlock(blockId, bcb, pos, color, 0, master);
    world->getBlockById(blockId)->blockCode->parseUserBlockElements(blockElt);
}

} // SmartBlocks namespace
