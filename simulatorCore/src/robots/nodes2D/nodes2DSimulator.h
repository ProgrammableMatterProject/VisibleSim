/**
 * @file   nodes2DSimulator.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:10:54 2019
 *
 * @brief
 *
 *
 */

#ifndef NODES2DSIMULATOR_H_
#define NODES2DSIMULATOR_H_

#include "base/simulator.h"
#include "robots/nodes2D/nodes2DBlockCode.h"
#include "robots/nodes2D/nodes2DWorld.h"
#include "utils/trace.h"

using namespace std;

namespace Nodes2D {

class Nodes2DSimulator : public BaseSimulator::Simulator {
protected:

    Nodes2DSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~Nodes2DSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static Nodes2DSimulator* getSimulator() {
        assert(simulator != NULL);
        return((Nodes2DSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
               int argc, char *argv[]) override;
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, bool master) override;
    virtual void printInfo() override { OUTPUT << "I'm a Nodes2DSimulator" << endl; }
    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    Nodes2DSimulator::createSimulator(argc, argv, bcb);
}

inline Nodes2DSimulator* getSimulator() { return(Nodes2DSimulator::getSimulator()); }

} // Nodes2D namespace
#endif /* NODES2DSIMULATOR_H_ */
