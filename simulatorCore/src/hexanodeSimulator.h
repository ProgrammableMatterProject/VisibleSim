/**
 * @file   nodeSimulator.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:10:54 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODESIMULATOR_H_
#define HEXANODESIMULATOR_H_

#include "simulator.h"
#include "hexanodeBlockCode.h"
#include "hexanodeWorld.h"
#include "trace.h"

using namespace std;

namespace Hexanode {

class HexanodeSimulator : public BaseSimulator::Simulator {
protected:

    HexanodeSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~HexanodeSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static HexanodeSimulator* getSimulator() {
        assert(simulator != NULL);
        return((HexanodeSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
               int argc, char *argv[]) override;
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, bool master) override;
    virtual void printInfo() override { OUTPUT << "I'm a HexanodeSimulator" << endl; }
    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    HexanodeSimulator::createSimulator(argc, argv, bcb);
}

inline HexanodeSimulator* getSimulator() { return(HexanodeSimulator::getSimulator()); }

} // Hexanode namespace
#endif /* HEXANODESIMULATOR_H_ */
