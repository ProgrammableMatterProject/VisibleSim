/*
 * slidingCubesSimulator.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoit
 */

#ifndef SLIDINGCUBESSIMULATOR_H_
#define SLIDINGCUBESSIMULATOR_H_

#include "../../base/simulator.h"
#include "slidingCubesBlockCode.h"
#include "slidingCubesWorld.h"
#include "../../utils/trace.h"

using namespace std;

namespace SlidingCubes {

    class SlidingCubesSimulator : public BaseSimulator::Simulator {
protected:

    SlidingCubesSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~SlidingCubesSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static SlidingCubesSimulator* getSimulator() {
        assert(simulator != NULL);
        return((SlidingCubesSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                           int argc, char *argv[]) override;
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, uint8_t orient) override;
    virtual void printInfo() override { OUTPUT << "I'm a SlidingCubesSimulator" << endl; }

    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    SlidingCubesSimulator::createSimulator(argc, argv, bcb);
}

inline SlidingCubesSimulator* getSimulator() { return(SlidingCubesSimulator::getSimulator()); }

} // SlidingCubes namespace
#endif /* SLIDINGCUBESSIMULATOR_H_ */
