/*
 * robotBlocksSimulator.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoit
 */

#ifndef ROBOTBLOCKSSIMULATOR_H_
#define ROBOTBLOCKSSIMULATOR_H_

#include "simulator.h"
#include "robotBlocksBlockCode.h"
#include "robotBlocksWorld.h"
#include "trace.h"

using namespace std;

namespace RobotBlocks {

class RobotBlocksSimulator : public BaseSimulator::Simulator {
protected:

    RobotBlocksSimulator(int argc, char *argv[], BlockCodeBuilder bcb);
    virtual ~RobotBlocksSimulator();

public:
    bool testMode;

    static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

    static RobotBlocksSimulator* getSimulator() {
        assert(simulator != NULL);
        return((RobotBlocksSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                           int argc, char *argv[]);
    virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, bool master);
    virtual void printInfo() { OUTPUT << "I'm a RobotBlocksSimulator" << endl; }

    void help();
};

inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb) {
    RobotBlocksSimulator::createSimulator(argc, argv, bcb);
}

inline RobotBlocksSimulator* getSimulator() { return(RobotBlocksSimulator::getSimulator()); }

} // RobotBlocks namespace
#endif /* ROBOTBLOCKSSIMULATOR_H_ */
