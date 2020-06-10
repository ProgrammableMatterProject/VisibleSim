/*
 * slidingCubesSimulator.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include <string.h>

#include "robots/slidingCubes/slidingCubesSimulator.h"
#include "utils/trace.h"

using namespace std;

namespace SlidingCubes {

void SlidingCubesSimulator::help() {
    cerr << "VisibleSim:" << endl;
    cerr << "Robot01" << endl;
    exit(EXIT_SUCCESS);
}

SlidingCubesSimulator::SlidingCubesSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
    : BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "SlidingCubesSimulator constructor" << TermColor::Reset << endl;
#endif
}

SlidingCubesSimulator::~SlidingCubesSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "SlidingCubesSimulator destructor" << TermColor::Reset <<endl;
#endif
}

void SlidingCubesSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    simulator =  new SlidingCubesSimulator(argc, argv, bcb);
    simulator->parseConfiguration(argc, argv);
    simulator->startSimulation();
}

void SlidingCubesSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                    int argc, char *argv[]) {
    world = new SlidingCubesWorld(gridSize, gridScale, argc,argv);

    if (GlutContext::GUIisEnabled)
        world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");

    World::setWorld(world);
}

void SlidingCubesSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                                     const Cell3DPosition &pos, const Color &color, bool master) {

    // Any additional configuration file parsing exclusive to this type of block should be performed
    //  here, using the blockElt TiXmlElement.

    // ...Parsing code...

    // Finally, add block to the world
    ((SlidingCubesWorld*)world)->addBlock(blockId, bcb, pos, color, 0, master);
    world->getBlockById(blockId)->blockCode->parseUserBlockElements(blockElt);
}

} // SlidingCubes namespace
