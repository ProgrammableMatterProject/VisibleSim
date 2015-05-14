/*
 * multiCoresSimulator.h
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#ifndef MULTICORESSIMULATOR_H_
#define MULTICORESSIMULATOR_H_

#include "simulator.h"
#include "multiCoresBlockCode.h"
#include "multiCoresScheduler.h"
#include "multiCoresWorld.h"
#include "multiCoresEvents.h"


using namespace std;

namespace MultiCores {

class MultiCoresSimulator : public BaseSimulator::Simulator {
protected:
	MultiCoresSimulator(int argc, char *argv[], MultiCoresBlockCode *(*multiCoreBlockCodeBuildingFunction)(MultiCoresBlock*));
	virtual ~MultiCoresSimulator();

public:
	static void createSimulator(int argc, char *argv[], MultiCoresBlockCode *(*multiCoreBlockCodeBuildingFunction)(MultiCoresBlock*));
	static void deleteSimulator();

	static MultiCoresBlockCode *(*buildNewBlockCode)(MultiCoresBlock*);

	static MultiCoresSimulator* getSimulator() {
		assert(simulator != NULL);
		return((MultiCoresSimulator*)simulator);
	}

	void printInfo() { cout << "I'm a MultiCoresSimulator" << endl; }
};

inline void createSimulator(int argc, char *argv[], MultiCoresBlockCode *(*multiCoreBlockCodeBuildingFunction)(MultiCoresBlock*)) {
	MultiCoresSimulator::createSimulator(argc, argv, multiCoreBlockCodeBuildingFunction);
}
inline void deleteSimulator() {
	MultiCoresSimulator::deleteSimulator();
}

inline MultiCoresSimulator* getSimulator() { return(MultiCoresSimulator::getSimulator()); }

} // MultiCores namespace
#endif /* MULTICORESSIMULATOR_H_ */
