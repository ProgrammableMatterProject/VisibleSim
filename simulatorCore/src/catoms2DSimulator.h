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
#include "catoms2DWorld.h"
#include "trace.h"

using namespace std;

namespace Catoms2D {

class Catoms2DSimulator : public BaseSimulator::Simulator {
protected:

    Catoms2DSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~Catoms2DSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static Catoms2DSimulator* getSimulator() {
	assert(simulator != NULL);
	return((Catoms2DSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
			   int argc, char *argv[]);
    virtual void loadBlock(TiXmlElement *blockElt, int blockId, BlockCodeBuilder bcb,
						   const Cell3DPosition &pos, const Color &color, bool master);
    virtual void printInfo() { OUTPUT << "I'm a Catoms2DSimulator" << endl; }
    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    Catoms2DSimulator::createSimulator(argc, argv, bcb);
}

inline Catoms2DSimulator* getSimulator() { return(Catoms2DSimulator::getSimulator()); }

} // Catoms2D namespace
#endif /* CATOMS2DSIMULATOR_H_ */
