/*
 * datomsSimulator.cpp
 *
 *  Created on: 28 janvier 2018
 *      Author: Benoît
 */

#include <iostream>
#include "datomsSimulator.h"
#include <string.h>
#include "trace.h"
#include "utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Datoms {

void DatomsSimulator::help() {
    cerr << "VisibleSim:" << endl;
    cerr << "Datoms" << endl;
    exit(EXIT_SUCCESS);
}

DatomsSimulator::DatomsSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
    : BaseSimulator::Simulator(argc, argv, bcb) {
    OUTPUT << TermColor::LifecycleColor << "DatomsSimulator constructor" << TermColor::Reset << endl;
}

DatomsSimulator::~DatomsSimulator() {
    OUTPUT << TermColor::LifecycleColor << "DatomsSimulator destructor" << TermColor::Reset <<endl;
}

void DatomsSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    simulator =  new DatomsSimulator(argc, argv, bcb);
    simulator->parseConfiguration(argc, argv);
    simulator->startSimulation();
}

void DatomsSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                      int argc, char *argv[]) {
    world = new DatomsWorld(gridSize, gridScale, argc, argv);

    if (GlutContext::GUIisEnabled)
        world->loadTextures("../../simulatorCore/resources/textures/latticeTextures");

    World::setWorld(world);
}

void DatomsSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                                  const Cell3DPosition &pos, const Color &color, bool master) {

    // Any additional configuration file parsing exclusive to this type of block should be performed
    //  here, using the blockElt TiXmlElement.

    // set the orientation
    short orientation = 0;
    const char *attr = blockElt->Attribute("orientation");
    if (attr) {
        orientation = atoi(attr);
        OUTPUT << "orientation: " << orientation << endl;
    }

    // set the compressed piston
    PistonId piston = AllPistonsOff;
    attr = blockElt->Attribute("piston");
    if (attr) {
        string str = attr;
      if (str=="2345") piston = Piston2345;
    }
    OUTPUT << "piston: " << piston << endl;

    // Finally, add block to the world
    ((DatomsWorld*)world)->addBlock(blockId, bcb, pos, color, orientation, master);
    world->getBlockById(blockId)->blockCode->parseUserBlockElements(blockElt);
}

} // Datoms namespace
