/*
 * catoms3DSimulator.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include <string.h>
#include "catoms3DSimulator.h"
#include "../../utils/trace.h"
#include "../../utils/utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Catoms3D {

    void Catoms3DSimulator::help() {
        cerr << "VisibleSim:" << endl;
        cerr << "Catoms3D" << endl;
        exit(EXIT_SUCCESS);
    }

    Catoms3DSimulator::Catoms3DSimulator(int argc, char *argv[], BlockCodeBuilder bcb)
            : BaseSimulator::Simulator(argc, argv, bcb) {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << TermColor::LifecycleColor << "Catoms3DSimulator constructor" << TermColor::Reset << endl;
#endif
    }

    Catoms3DSimulator::~Catoms3DSimulator() {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << TermColor::LifecycleColor << "Catoms3DSimulator destructor" << TermColor::Reset <<endl;
#endif
    }

    void Catoms3DSimulator::createSimulator(int argc, char *argv[], BlockCodeBuilder bcb,
                                            bool useSkewedFCCLattice) {
        simulator = new Catoms3DSimulator(argc, argv, bcb);
        ((Catoms3DSimulator *) simulator)->useSkewedFCCLattice = useSkewedFCCLattice;
        cout << "createSimulator:" << int(((Catoms3DSimulator *) simulator)->useSkewedFCCLattice) << endl;
        simulator->parseConfiguration(argc, argv);
        simulator->startSimulation();
    }

    void Catoms3DSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                      int argc, char *argv[]) {
        world = new Catoms3DWorld(gridSize, gridScale, argc, argv);
#ifdef WIN32
        string directory = string(ROOT_DIR) + "/simulatorCore/resources/textures/latticeTextures";
#else
        string directory = "../../simulatorCore/resources/textures/latticeTextures";
#endif
        if (GlutContext::GUIisEnabled)
            world->loadTextures(directory);

        World::setWorld(world);
    }

    void Catoms3DSimulator::loadBlock(TiXmlElement *blockElt, bID blockId,
                                      BlockCodeBuilder bcb, const Cell3DPosition &pos,
                                      const Color &color, uint8_t orient) {

        // Any additional configuration file parsing exclusive to this type of block should be performed
        //  here, using the blockElt TiXmlElement.
        if (orient > 23) {
            uniform_int_distribution<short> distribution(0, 23);
            orient = distribution(generator);
        }

        // Finally, add block to the world
        ((Catoms3DWorld *) world)->addBlock(blockId, bcb, pos, color, orient);
        world->getBlockById(blockId)->blockCode->parseUserBlockElements(blockElt);
    }

} // Catoms3D namespace
