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
#include "commandLine.h"

using namespace std;

namespace BaseSimulator {

class Simulator;

extern Simulator *simulator;

class Simulator {
public:
	enum Type {CPP = 0, MELDPROCESS = 1};
	static Simulator* getSimulator() {
		assert(simulator != NULL);
		return(simulator);
	}
	
	inline static void setType (Type t) { type = t; };
	inline static Type getType () { return type; };
	inline CommandLine& getCmdLine() { return cmdLine; }
		
	virtual void printInfo() { cout << "I'm a Simulator" << endl; }

protected:
	static Type type;
	
	static Simulator *simulator;
	static Scheduler *scheduler;

	TiXmlDocument *xmlDoc;
	TiXmlNode* xmlWorldNode;
	
	CommandLine cmdLine;
	
	Simulator(int argc, char *argv[]);
	virtual ~Simulator();
};
} // Simulator namespace

#endif /* SIMULATOR_H_ */
