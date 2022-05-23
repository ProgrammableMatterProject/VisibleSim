/*
 * smartBlocksSimulator.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef SMARTBLOCKSSIMULATOR_H_
#define SMARTBLOCKSSIMULATOR_H_

#include "smartBlocksWorld.h"
#include "../../base/simulator.h"
#include "smartBlocksBlock.h"
#include "smartBlocksBlockCode.h"

using namespace std;

namespace SmartBlocks {

class SmartBlocksSimulator : public BaseSimulator::Simulator {
protected:
    SmartBlocksSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~SmartBlocksSimulator();

public:
    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static SmartBlocksSimulator* getSimulator() {
        assert(simulator != NULL);
        return((SmartBlocksSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                           int argc, char *argv[]) override;
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, uint8_t orient) override;
    virtual void printInfo() override { cout << "I'm a SmartBlocksSimulator" << endl; }
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    SmartBlocksSimulator::createSimulator(argc, argv, bcb);
}

inline SmartBlocksSimulator* getSimulator() { return(SmartBlocksSimulator::getSimulator()); }

} // SmartBlocks namespace

#endif /* SMARTBLOCKSSIMULATOR_H_ */
