/*
 * datomSimulator.h
 *
 *  Created on: 28 janvier 2018
 *      Author: Benoit
 */

#ifndef DATOMSSIMULATOR_H_
#define DATOMSSIMULATOR_H_

#include "../../base/simulator.h"
#include "datomsBlockCode.h"
#include "datomsWorld.h"
#include "../../utils/trace.h"

using namespace std;

namespace Datoms {

class DatomsSimulator : public BaseSimulator::Simulator {
protected:

    DatomsSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~DatomsSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static DatomsSimulator* getSimulator() {
        assert(simulator != NULL);
        return((DatomsSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
               int argc, char *argv[]) override;
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, uint8_t orient) override;
    virtual void printInfo() override { OUTPUT << "I'm a DatomsSimulator" << endl; }
    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    DatomsSimulator::createSimulator(argc, argv, bcb);
}

inline DatomsSimulator* getSimulator() { return(DatomsSimulator::getSimulator()); }

} // Datoms namespace
#endif /* DATOMSIMULATOR_H_ */
