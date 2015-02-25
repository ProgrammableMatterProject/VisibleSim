/*
 * simulator.h
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#ifndef SIMULATOR_H_
#define SIMULATOR_H_

#define TIXML_USE_STL	1
#include "TinyXML/tinyxml.h"

#include "scheduler.h"
#include "world.h"

using namespace std;

namespace BaseSimulator {

class Simulator;

extern Simulator *simulator;

class Simulator {
protected:
	static Simulator *simulator;
	static Scheduler *scheduler;

	TiXmlDocument *xmlDoc;
	TiXmlNode* xmlWorldNode;

	Simulator(int argc, char *argv[]);
	
	virtual ~Simulator();

public:
	static Simulator* getSimulator() {
		assert(simulator != NULL);
		return(simulator);
	}
	
	virtual void printInfo() { cout << "I'm a Simulator" << endl; }
};
} // Simulator namespace

#endif /* SIMULATOR_H_ */
