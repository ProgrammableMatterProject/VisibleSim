/*!
 * \file oktenSimulator.h
 * \brief okten Simulator
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#ifndef OKTENSIMULATOR_H_
#define OKTENSIMULATOR_H_

#include "simulator.h"
#include "oktenBlockCode.h"
#include "oktenWorld.h"
#include "trace.h"

using namespace std;

namespace Okten {

class OktenSimulator : public BaseSimulator::Simulator {
protected:

    OktenSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~OktenSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static OktenSimulator* getSimulator() {
        assert(simulator != NULL);
        return((OktenSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
			   int argc, char *argv[]);
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
						   const Cell3DPosition &pos, const Color &color, bool master);
    virtual void printInfo() { OUTPUT << "I'm a OktenSimulator" << endl; }
    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    OktenSimulator::createSimulator(argc, argv, bcb);
}

inline OktenSimulator* getSimulator() { return(OktenSimulator::getSimulator()); }

} // Okten namespace
#endif /* OKTENSIMULATOR_H_ */
