/*
 * smartBlocksSimulator.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef SMARTBLOCKSSIMULATOR_H_
#define SMARTBLOCKSSIMULATOR_H_

#include "smartBlocksWorld.h"
#include "simulator.h"
#include "smartBlocksBlock.h"
#include "smartBlocksBlockCode.h"

using namespace std;

namespace SmartBlocks {

class SmartBlocksSimulator : public BaseSimulator::Simulator {
protected:
	SmartBlocksSimulator(int argc, char *argv[],
						 SmartBlocksBlockCode *(*smartBlocksCodeBuildingFunction)(SmartBlocksBlock*));
	virtual ~SmartBlocksSimulator();

public:
	static void createSimulator(int argc, char *argv[],
								SmartBlocksBlockCode *(*smartBlocksBlockCodeBuildingFunction)
								(SmartBlocksBlock*));   

	static SmartBlocksSimulator* getSimulator() {
		assert(simulator != NULL);
		return((SmartBlocksSimulator*)simulator);
	}

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
						   int argc, char *argv[]);
	virtual void loadBlock(TiXmlElement *blockElt, int blockId, BlockCode *(*buildingBlockCodeBuildingFunction)
						   (BuildingBlock*), const Cell3DPosition &pos, const Color &color, bool master);
	virtual void printInfo() { cout << "I'm a SmartBlocksSimulator" << endl; }
};

inline void createSimulator(int argc, char *argv[],
							SmartBlocksBlockCode *(*smartBlocksBlockCodeBuildingFunction)(SmartBlocksBlock*)) {
	SmartBlocksSimulator::createSimulator(argc, argv, smartBlocksBlockCodeBuildingFunction);
}

inline SmartBlocksSimulator* getSimulator() { return(SmartBlocksSimulator::getSimulator()); }

} // SmartBlocks namespace

#endif /* SMARTBLOCKSSIMULATOR_H_ */
