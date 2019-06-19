/**
 * @file   nodeSimulator.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:10:54 2019
 *
 * @brief
 *
 *
 */

#ifndef NODESIMULATOR_H_
#define NODESIMULATOR_H_

#include "simulator.h"
#include "nodeBlockCode.h"
#include "nodeWorld.h"
#include "trace.h"

using namespace std;

namespace Node {

class NodeSimulator : public BaseSimulator::Simulator {
protected:

    NodeSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~NodeSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static NodeSimulator* getSimulator() {
        assert(simulator != NULL);
        return((NodeSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
               int argc, char *argv[]) override;
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, bool master) override;
    virtual void printInfo() override { OUTPUT << "I'm a NodeSimulator" << endl; }
    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    NodeSimulator::createSimulator(argc, argv, bcb);
}

inline NodeSimulator* getSimulator() { return(NodeSimulator::getSimulator()); }

} // Node namespace
#endif /* NODESIMULATOR_H_ */
