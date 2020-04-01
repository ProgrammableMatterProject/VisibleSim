/*!
 * \file okteenSimulator.h
 * \brief okteen Simulator
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#ifndef OKTEENSIMULATOR_H_
#define OKTEENSIMULATOR_H_

#include "simulator.h"
#include "okteenBlockCode.h"
#include "okteenWorld.h"
#include "trace.h"

using namespace std;

namespace Okteen {

class OkteenSimulator : public BaseSimulator::Simulator {
protected:

    OkteenSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~OkteenSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static OkteenSimulator* getSimulator() {
        assert(simulator != NULL);
        return((OkteenSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
               int argc, char *argv[]) override;
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, bool master) override;
    virtual void printInfo() override { OUTPUT << "I'm a OkteenSimulator" << endl; }
    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    OkteenSimulator::createSimulator(argc, argv, bcb);
}

inline OkteenSimulator* getSimulator() { return(OkteenSimulator::getSimulator()); }

} // Okteen namespace
#endif /* OKTEENSIMULATOR_H_ */
