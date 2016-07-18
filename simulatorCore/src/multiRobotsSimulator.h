/*
 * @file multiRobotsSimulator.h
 *
 *  Created on: 14/07/16
 *      Author: pthalamy
 */

#ifndef MULTIROBOTSSIMULATOR_H_
#define MULTIROBOTSSIMULATOR_H_

#include "simulator.h"
#include "multiRobotsBlockCode.h"
#include "multiRobotsWorld.h"
#include "trace.h"

using namespace std;

namespace MultiRobots {

class MultiRobotsSimulator : public BaseSimulator::Simulator {
protected:

    MultiRobotsSimulator(int argc, char *argv[],
                         MultiRobotsBlockCode *(*blinkyBlocksCodeBuildingFunction)(MultiRobotsBlock*));
    virtual ~MultiRobotsSimulator();

public:

    static void createSimulator(int argc, char *argv[],
                                MultiRobotsBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)
                                (MultiRobotsBlock*));   

    static MultiRobotsSimulator* getSimulator() {
	assert(simulator != NULL);
	return((MultiRobotsSimulator*)simulator);
    }

    virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
		   int argc, char *argv[]);
    virtual void loadBlock(TiXmlElement *blockElt, int blockId, BlockCodeBuilder bcb,
                           const Cell3DPosition &pos, const Color &color, bool master);
    virtual void printInfo() { OUTPUT << "I'm a MultiRobotsSimulator" << endl; }
};

inline void createSimulator(int argc, char *argv[], MultiRobotsBlockCode *(*bcb)(MultiRobotsBlock*)) {
    MultiRobotsSimulator::createSimulator(argc, argv, bcb);
}

inline MultiRobotsSimulator* getSimulator() { return(MultiRobotsSimulator::getSimulator()); }

} // MultiRobots namespace
#endif /* MULTIROBOTSSIMULATOR_H_ */
