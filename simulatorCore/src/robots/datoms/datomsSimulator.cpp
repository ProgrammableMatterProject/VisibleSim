/*
 * datomsSimulator.cpp
 *
 *  Created on: 28 janvier 2018
 *      Author: Benoît
 */

#include <iostream>
#include <string.h>
#include "../../utils/trace.h"
#include "../../utils/utils.h"
#include "datomsSimulator.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Datoms {

#ifdef WIN32
    string textureDirectory = string(ROOT_DIR) + "/simulatorCore/resources/textures/";
#else
    string textureDirectory = "../../simulatorCore/resources/textures/";
#endif

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
        world->loadTextures(textureDirectory+"/latticeTextures");

    World::setWorld(world);
}

void DatomsSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                                  const Cell3DPosition &pos, const Color &color, uint8_t orient) {

    // Any additional configuration file parsing exclusive to this type of block should be performed
    //  here, using the blockElt TiXmlElement.

    // set the orientation
    short rotCode = 0;
    const char *attr = blockElt->Attribute("orientation");
    if (attr) {
        rotCode = atoi(attr);
        OUTPUT << "orientation: " << rotCode << endl;
    }

    // set the compressed piston
    PistonId piston = AllPistonsOff;
    attr = blockElt->Attribute("piston");
    if (attr) {
        string str = attr;
        if (str=="2345") piston = Piston2345;
    }
    OUTPUT << "piston: " << piston << endl;
    rotCode+=piston*64;

    // Finally, add block to the world
    ((DatomsWorld*)world)->addBlock(blockId, bcb, pos, color, rotCode);
    world->getBlockById(blockId)->blockCode->parseUserBlockElements(blockElt);
}

} // Datoms namespace
