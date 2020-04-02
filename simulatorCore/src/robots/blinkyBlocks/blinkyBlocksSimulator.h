/*
 * blinkyBlocksSimulator.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef BLINKYBLOCKSSIMULATOR_H_
#define BLINKYBLOCKSSIMULATOR_H_

#include "base/simulator.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "utils/trace.h"

using namespace std;

namespace BlinkyBlocks {

class BlinkyBlocksSimulator : public BaseSimulator::Simulator {
protected:

    BlinkyBlocksSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~BlinkyBlocksSimulator();

public:

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static BlinkyBlocksSimulator* getSimulator() {
    assert(simulator != NULL);
    return((BlinkyBlocksSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
           int argc, char *argv[]) override;
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, bool master) override;
    virtual void parseScenario();
    virtual void printInfo() override { OUTPUT << "I'm a BlinkyBlocksSimulator" << endl; }
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    BlinkyBlocksSimulator::createSimulator(argc, argv, bcb);
}

inline BlinkyBlocksSimulator* getSimulator() { return(BlinkyBlocksSimulator::getSimulator()); }

} // BlinkyBlocks namespace
#endif /* BLINKYBLOCKSSIMULATOR_H_ */
