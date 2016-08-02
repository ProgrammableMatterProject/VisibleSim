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
#include "catoms3DWorld.h"
#include "trace.h"

using namespace std;

namespace Catoms3D {

class Catoms3DSimulator : public BaseSimulator::Simulator {
protected:

    Catoms3DSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~Catoms3DSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static Catoms3DSimulator* getSimulator() {
	assert(simulator != NULL);
	return((Catoms3DSimulator*)simulator);
    }
   
    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
			   int argc, char *argv[]);
    virtual void loadBlock(TiXmlElement *blockElt, int blockId, BlockCodeBuilder bcb,
						   const Cell3DPosition &pos, const Color &color, bool master);
    virtual void parseSkeleton();
    virtual void printInfo() { OUTPUT << "I'm a Catoms3DSimulator" << endl; }
    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    Catoms3DSimulator::createSimulator(argc, argv, bcb);
}

inline Catoms3DSimulator* getSimulator() { return(Catoms3DSimulator::getSimulator()); }

} // Catoms3D namespace
#endif /* CATOMS3DSIMULATOR_H_ */
