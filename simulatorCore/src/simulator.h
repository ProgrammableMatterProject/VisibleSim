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
#include "BlockCode.h"

using namespace std;

namespace BaseSimulator {

class Simulator;

extern Simulator *simulator;

class Simulator {
public:
	enum Type {CPP = 0, MELDPROCESS = 1, MELDINTERPRET = 2};
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

	static Simulator *simulator; //!< Static member for accessing *this* simulator
	Scheduler *scheduler;		//!< Scheduler to be instantiated and configured

	World *world; 				//!< Simulation world to be instantiated and configured
	
	TiXmlDocument *xmlDoc;		//!< TinyXMLDocument for the configuration file
	TiXmlNode* xmlWorldNode; //!< world XML node from the configuration file

	BlockCode *(*newBlockCode)(BuildingBlock*); //!< Function pointer to the target BlockCode
	
	CommandLine cmdLine;		//!< Utility member for accessing command line arguments

	void parseWorld(int argc, char*argv[]);
	void parseBlockList();
	void parseTarget(int yz);   
	
	virtual void loadWorld(int lx, int ly, int lz, int argc, char *argv[]) = 0;	
	virtual void loadScheduler() = 0;   
	virtual void loadBlock(TiXmlElement *blockElt, int blockId, BlockCode *(*buildingBlockCodeBuildingFunction)
						   (BuildingBlock*), const Cell3DPosition &pos,
						   const Color &color, bool master) {};
	virtual void loadTargetAndCapabilities(vector<Cell3DPosition> targetCells) {};
	
	Simulator(int argc, char *argv[]);
	virtual ~Simulator();
};
} // Simulator namespace

#endif /* SIMULATOR_H_ */
