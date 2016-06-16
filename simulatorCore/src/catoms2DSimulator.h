/*
 * catoms2DSimulator.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoit
 */

#ifndef CATOMS2DSIMULATOR_H_
#define CATOMS2DSIMULATOR_H_

#include "simulator.h"
#include "catoms2DBlockCode.h"
#include "catoms2DScheduler.h"
#include "catoms2DWorld.h"
#include "trace.h"

using namespace std;

namespace Catoms2D {

class Catoms2DSimulator : public BaseSimulator::Simulator {
protected:

	Catoms2DSimulator(int argc, char *argv[],
			  Catoms2DBlockCode *(*catoms2DCodeBuildingFunction)(Catoms2DBlock*));
	virtual ~Catoms2DSimulator();

public:
	bool testMode;

	static void createSimulator(int argc, char *argv[],
				    Catoms2DBlockCode *(*catoms2DBlockCodeBuildingFunction)(Catoms2DBlock*));
	static void deleteSimulator();

	static Catoms2DBlockCode *(*buildNewBlockCode)(Catoms2DBlock*);

	static Catoms2DSimulator* getSimulator() {
		assert(simulator != NULL);
		return((Catoms2DSimulator*)simulator);
	}

	void loadScheduler();
	void loadWorld(int lx, int ly, int lz, int argc, char *argv[]);
	void loadBlock(TiXmlElement *blockElt, int blockId, BlockCode *(*buildingBlockCodeBuildingFunction)
		       (BuildingBlock*), const Cell3DPosition &pos, const Color &color, bool master);
	void loadTargetAndCapabilities(vector<Cell3DPosition> targetCells);
		
	void printInfo() { OUTPUT << "I'm a Catoms2DSimulator" << endl; }
	void help();
};

inline void createSimulator(int argc, char *argv[],
			    Catoms2DBlockCode *(*catoms2DBlockCodeBuildingFunction)(Catoms2DBlock*)) {
	Catoms2DSimulator::createSimulator(argc, argv, catoms2DBlockCodeBuildingFunction);
}

inline void deleteSimulator() {
	Catoms2DSimulator::deleteSimulator();
}

inline Catoms2DSimulator* getSimulator() { return(Catoms2DSimulator::getSimulator()); }

} // Catoms2D namespace
#endif /* CATOMS2DSIMULATOR_H_ */
