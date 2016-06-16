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
#include "robotBlocksScheduler.h"
#include "robotBlocksWorld.h"
#include "trace.h"

using namespace std;

namespace RobotBlocks {

class RobotBlocksSimulator : public BaseSimulator::Simulator {
protected:

	RobotBlocksSimulator(int argc, char *argv[], RobotBlocksBlockCode *(*robotBlocksCodeBuildingFunction)(RobotBlocksBlock*));
	virtual ~RobotBlocksSimulator();

public:
   bool testMode;

	static void createSimulator(int argc, char *argv[], RobotBlocksBlockCode *(*robotBlocksBlockCodeBuildingFunction)(RobotBlocksBlock*));
	static void deleteSimulator();

	static RobotBlocksBlockCode *(*buildNewBlockCode)(RobotBlocksBlock*);

	static RobotBlocksSimulator* getSimulator() {
		assert(simulator != NULL);
		return((RobotBlocksSimulator*)simulator);
	}

	void loadScheduler();
	void loadWorld(int lx, int ly, int lz, int argc, char *argv[]);
	
	void printInfo() { OUTPUT << "I'm a RobotBlocksSimulator" << endl; }

   void help();
};

inline void createSimulator(int argc, char *argv[], RobotBlocksBlockCode *(*robotBlocksBlockCodeBuildingFunction)(RobotBlocksBlock*)) {
	RobotBlocksSimulator::createSimulator(argc, argv, robotBlocksBlockCodeBuildingFunction);
}

inline void deleteSimulator() {
	RobotBlocksSimulator::deleteSimulator();
}

inline RobotBlocksSimulator* getSimulator() { return(RobotBlocksSimulator::getSimulator()); }

} // RobotBlocks namespace
#endif /* ROBOTBLOCKSSIMULATOR_H_ */
