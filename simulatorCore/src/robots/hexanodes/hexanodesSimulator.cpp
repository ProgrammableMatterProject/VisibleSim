/**
 * @file   HexanodesSimulator.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:10:11 2019
 *
 * @brief
 *
 *
 */


#include <iostream>
#include <cstring>

#include "hexanodesSimulator.h"
#include "../../utils/trace.h"
#include "../../utils/utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Hexanodes {

#ifdef WIN32
    string textureDirectory = string(ROOT_DIR) + "/simulatorCore/resources/textures/";
#else
    string textureDirectory = "../../simulatorCore/resources/textures/";
#endif
void HexanodesSimulator::help() {
    cerr << "VisibleSim:" << endl;
    cerr << "Hexanodes" << endl;
    exit(EXIT_SUCCESS);
}

HexanodesSimulator::HexanodesSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
    : BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "HexanodesSimulator constructor" << TermColor::Reset << endl;
#endif
}

HexanodesSimulator::~HexanodesSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << TermColor::LifecycleColor << "HexanodesSimulator destructor" << TermColor::Reset <<endl;
#endif
}

void HexanodesSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    simulator =  new HexanodesSimulator(argc, argv, bcb);
    simulator->parseConfiguration(argc, argv);
    simulator->startSimulation();
}

void HexanodesSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                   int argc, char *argv[]) {
    world = new HexanodesWorld(gridSize, gridScale, argc, argv);

    if (GlutContext::GUIisEnabled)
        world->loadTextures(textureDirectory+"/latticeTextures");

    World::setWorld(world);
}

void HexanodesSimulator::loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                                  const Cell3DPosition &pos, const Color &color, uint8_t orient) {

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
    ((HexanodesWorld*)world)->addBlock(blockId, bcb, pos, color, orientation);
    world->getBlockById(blockId)->blockCode->parseUserBlockElements(blockElt);
}

} // Hexanodes namespace
