/*
 * catoms3DSimulator.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoit
 */

#ifndef CATOMS3DSIMULATOR_H_
#define CATOMS3DSIMULATOR_H_

#include "simulator.h"
#include "catoms3DBlockCode.h"
#include "catoms3DScheduler.h"
#include "catoms3DWorld.h"
#include "trace.h"

using namespace std;

namespace Catoms3D {

class Catoms3DSimulator : public BaseSimulator::Simulator {
protected:

	Catoms3DSimulator(int argc, char *argv[],
			  Catoms3DBlockCode *(*catoms3DCodeBuildingFunction)(Catoms3DBlock*));
	virtual ~Catoms3DSimulator();

public:
	bool testMode;

	static void createSimulator(int argc, char *argv[],
				    Catoms3DBlockCode *(*catoms3DBlockCodeBuildingFunction)(Catoms3DBlock*));
	static void deleteSimulator();

	static Catoms3DBlockCode *(*buildNewBlockCode)(Catoms3DBlock*);

	static Catoms3DSimulator* getSimulator() {
		assert(simulator != NULL);
		return((Catoms3DSimulator*)simulator);
	}

	void loadScheduler();
	void loadWorld(int lx, int ly, int lz, int argc, char *argv[]);
	void loadBlock(TiXmlElement *blockElt, int blockId, BlockCode *(*buildingBlockCodeBuildingFunction)
		       (BuildingBlock*), const Cell3DPosition &pos, const Color &color, bool master);
	void parseSkeleton();

	void printInfo() { OUTPUT << "I'm a Catoms3DSimulator" << endl; }
	void help();
};

inline void createSimulator(int argc, char *argv[],
			    Catoms3DBlockCode *(*catoms3DBlockCodeBuildingFunction)(Catoms3DBlock*)) {
	Catoms3DSimulator::createSimulator(argc, argv, catoms3DBlockCodeBuildingFunction);
}

inline void deleteSimulator() {
	Catoms3DSimulator::deleteSimulator();
}

inline Catoms3DSimulator* getSimulator() { return(Catoms3DSimulator::getSimulator()); }

} // Catoms3D namespace
#endif /* CATOMS3DSIMULATOR_H_ */
