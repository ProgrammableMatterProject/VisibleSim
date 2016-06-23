/*
 * blinkyBlocksSimulator.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef BLINKYBLOCKSSIMULATOR_H_
#define BLINKYBLOCKSSIMULATOR_H_

#include "simulator.h"
#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksWorld.h"
#include "trace.h"

using namespace std;

namespace BlinkyBlocks {

class BlinkyBlocksSimulator : public BaseSimulator::Simulator {
protected:

    BlinkyBlocksSimulator(int argc, char *argv[], BlinkyBlocksBlockCode *(*blinkyBlocksCodeBuildingFunction)(BlinkyBlocksBlock*));
    virtual ~BlinkyBlocksSimulator();

public:

    static void createSimulator(int argc, char *argv[], BlinkyBlocksBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(BlinkyBlocksBlock*));

    static BlinkyBlocksBlockCode *(*buildNewBlockCode)(BlinkyBlocksBlock*);

    static BlinkyBlocksSimulator* getSimulator() {
	assert(simulator != NULL);
	return((BlinkyBlocksSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
		   int argc, char *argv[]);
    virtual void loadBlock(TiXmlElement *blockElt, int blockId, BlockCode *(*buildingBlockCodeBuildingFunction)
			   (BuildingBlock*), const Cell3DPosition &pos, const Color &color, bool master);
    virtual void parseScenario();
    virtual void printInfo() { OUTPUT << "I'm a BlinkyBlocksSimulator" << endl; }
};

inline void createSimulator(int argc, char *argv[], BlinkyBlocksBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(BlinkyBlocksBlock*)) {
    BlinkyBlocksSimulator::createSimulator(argc, argv, blinkyBlocksBlockCodeBuildingFunction);
}

inline BlinkyBlocksSimulator* getSimulator() { return(BlinkyBlocksSimulator::getSimulator()); }

} // BlinkyBlocks namespace
#endif /* BLINKYBLOCKSSIMULATOR_H_ */
