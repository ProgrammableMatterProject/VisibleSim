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
#include "blinkyBlocksScheduler.h"
#include "blinkyBlocksWorld.h"
#include "trace.h"

using namespace std;

namespace BlinkyBlocks {

class BlinkyBlocksSimulator : public BaseSimulator::Simulator {
protected:

	BlinkyBlocksSimulator(int argc, char *argv[], BlinkyBlocksBlockCode *(*blinkyBlocksCodeBuildingFunction)(BlinkyBlocksBlock*));
	virtual ~BlinkyBlocksSimulator();

public:
   bool testMode;

	static void createSimulator(int argc, char *argv[], BlinkyBlocksBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(BlinkyBlocksBlock*));
	static void deleteSimulator();

	static BlinkyBlocksBlockCode *(*buildNewBlockCode)(BlinkyBlocksBlock*);

	static BlinkyBlocksSimulator* getSimulator() {
		assert(simulator != NULL);
		return((BlinkyBlocksSimulator*)simulator);
	}
   
   

	void printInfo() { OUTPUT << "I'm a BlinkyBlocksSimulator" << endl; }
   
   void help();
};

inline void createSimulator(int argc, char *argv[], BlinkyBlocksBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(BlinkyBlocksBlock*)) {
	BlinkyBlocksSimulator::createSimulator(argc, argv, blinkyBlocksBlockCodeBuildingFunction);
}
inline void deleteSimulator() {
	BlinkyBlocksSimulator::deleteSimulator();
}

inline BlinkyBlocksSimulator* getSimulator() { return(BlinkyBlocksSimulator::getSimulator()); }

} // BlinkyBlocks namespace
#endif /* BLINKYBLOCKSSIMULATOR_H_ */
