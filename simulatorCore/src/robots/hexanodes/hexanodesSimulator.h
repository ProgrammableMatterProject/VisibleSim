/**
 * @file   nodeSimulator.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:10:54 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODESSIMULATOR_H_
#define HEXANODESSIMULATOR_H_

#include "../../base/simulator.h"
#include "hexanodesBlockCode.h"
#include "hexanodesWorld.h"
#include "../../utils/trace.h"

using namespace std;

namespace Hexanodes {

class HexanodesSimulator : public BaseSimulator::Simulator {
protected:

    HexanodesSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~HexanodesSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static HexanodesSimulator* getSimulator() {
        assert(simulator != NULL);
        return((HexanodesSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
               int argc, char *argv[]) override;
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, uint8_t orient) override;
    virtual void printInfo() override { OUTPUT << "I'm a HexanodesSimulator" << endl; }
    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    HexanodesSimulator::createSimulator(argc, argv, bcb);
}

inline HexanodesSimulator* getSimulator() { return(HexanodesSimulator::getSimulator()); }

} // Hexanodes namespace
#endif /* HEXANODESSIMULATOR_H_ */
