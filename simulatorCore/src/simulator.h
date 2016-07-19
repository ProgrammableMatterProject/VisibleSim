/*! @file simulator.h
 *  @brief Header file for the Abstract class Simulator
 *
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
#include "blockCode.h"

using namespace std;

namespace BaseSimulator {

class Simulator;

extern Simulator *simulator;

/*! @class Simulator
 *  @brief Simulator is responsible for creating and configuring the core components of simulation (i.e. World, Scheduler), by parsing the configuration file and interpreting the command line args
 *
 */
class Simulator {
public:
	enum Type {CPP = 0, MELDPROCESS = 1, MELDINTERPRET = 2};

	static Simulator* getSimulator() {
		assert(simulator != NULL);
		return(simulator);
	}

	/*! 
	 *  @brief Statically deletes this instance of the simulator
	 */
	static void deleteSimulator();

	inline static void setType (Type t) { type = t; };
	inline static Type getType () { return type; };
	inline CommandLine& getCmdLine() { return cmdLine; }

	virtual void printInfo() { cout << "I'm a Simulator" << endl; }

	/*! 
	 *  @brief Successively calls all configuration file parsing functions to configure the simulation
	 *
	 *  @param argc The number of command line arguments
	 *  @param argv The command line arguments
	 */
	void parseConfiguration(int argc, char*argv[]);

	/*! 
	 *  @brief Starts the simulation 
	 *   (i.e. link the blocks, start the scheduler if needed, and enter the GLUT main loop)
	 *
	 */
	void startSimulation(void);

protected:
	static Type type;			//!< Type of simulation, i.e. language of the user program
	
	static Simulator *simulator; //!< Static member for accessing *this* simulator
	Scheduler *scheduler;		//!< Scheduler to be instantiated and configured
	World *world;				//!< Simulation world to be instantiated and configured

	TiXmlDocument *xmlDoc;		//!< TinyXMLDocument for the configuration file
	TiXmlNode* xmlWorldNode; //!< world XML node from the configuration file
	TiXmlNode* xmlBlockListNode; //!< blockList XML node from the configuration file

	BlockCodeBuilder bcb; //!< Function pointer to the target BlockCode builder

	CommandLine cmdLine;		//!< Utility member for accessing command line arguments
	int schedulerMaxDate = 0;		//!< Maximum simulation date
	
	/*! 
	 *  @brief Identify the type of the simulation (CPP / Meld Process / Meld Interpreter) 
	 *   from the command line
	 *
	 *  @param argc The number of command line arguments
	 *  @param argv The command line arguments
	 *
	 */
	void readSimulationType(int argc, char*argv[]);

	/*!
	 *  @brief Parses the configuration file for World information common to all blocks.
	 *
	 *  Calls the loadWorld virtual function to instantiate the right subclass of World with the parsed data.
	 *
	 *  @param argc The number of command line arguments
	 *  @param argv The command line arguments
	 *
	 */
	void parseWorld(int argc, char*argv[]);

	/*! @fn loadScheduler(int maximumDate)
	 *  @brief Instantiates a scheduler instance for the simulation based on the type of CodeBlock
	 *
	 *  MeldProcessScheduler, MeldInterpretScheduler, or CPPScheduler
	 *
	 *  @param maximumDate : maximum simulation date none by default
	 *
	 */
	void loadScheduler(int maximumDate = 0);

	/*! @fn parseCameraAndSpotlight();
	 *  @brief Parses the configuration file for Camera and Spotlight information
	 *
	 *  Calls the loadWorld virtual function to instantiate the right subclass of World with the parsed data.
	 *
	 *
	 */
	void parseCameraAndSpotlight();

	/*! @fn parseBlockList()
	 *  @brief Parses the configuration for block information common to all blocks
	 *
	 *  Calls the loadBlock virtual function once for every node to instantiate.
	 *
	 */
	void parseBlockList();

	/*! @fn parseTarget(yz)
	 *  @brief Parses the configuration for target information common to all blocks
	 *  @deprecated {}
	 *
	 *  Stores the positions of all cells to add to the target in a vector, then
	 *   calls the loadTargetAndCapabilities virual function to configure the target
	 *   attribute of the target world.
	 *
	 *  N.B.: In configuration file: the **line** attribute is for **y** and **plane** for **z**
	 *
	 */
	void parseTarget();
	
	/*! 
	 *  @brief Parses the configuration for obstacles information 
	 *
	 */
	void parseObstacles();
	
	/*! @fn virtual void loadWorld(int lx, int ly, int lz, int argc, char *argv[])
	 *  @brief Calls the createWorld function from the target world subclass to instantiate it
	 *
	 *  @param gridSize the size of the simulation grid
	 *  @param gridScale the real size of a block
	 *  @param argc The number of command line arguments
	 *  @param argv The command line arguments
	 *
	 */
	virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
						   int argc, char *argv[]) = 0;

	/*! @fn virtual void loadBlock(TiXmlElement *blockElt, int blockId, BlockCode *(*buildingBlockCodeBuildingFunction)(BuildingBlock*), const Cell3DPosition &pos, const Color &color, bool master)
	 *  @brief Parses the config file for any required additional block attribute, and add it to the world
	 *
	 *  @param blockElt The current block XML element for parsing additional attributes
	 *  @param master id of the block to add
	 *  @param buildingBlockCodeBuildingFunction function pointer to the user blockCode
	 *  @param pos Position of the block to add
	 *  @param master True if the block is a master block, false otherwise
	 *
	 */
	virtual void loadBlock(TiXmlElement *blockElt, int blockId,
						   BlockCode *(*buildingBlockCodeBuildingFunction)
						   (BuildingBlock*), const Cell3DPosition &pos,
						   const Color &color, bool master) {};

	/*! @fn virtual void loadTargetAndCapabilities(vector<Cell3DPosition> targetCells)
	 *  @brief Configures the target from the simulation World with the positions in targetCells
	 *
	 *  @param targetCells vector containing all cell positions to add to the target
	 *  @deprecated { Used for legacy targets and capabilities, moved to the user program space. }
	 *
	 */
	virtual void loadTargetAndCapabilities(vector<Cell3DPosition> targetCells) {};	
	
	Simulator(int argc, char *argv[], BlockCodeBuilder bcb);
	virtual ~Simulator();
public:
	/**
	 *  @brief Getter for the configuration file TiXmlDocument
	 *  @return a pointer to the configuration file TinyXml doc 
	 */
	inline TiXmlDocument *getConfigDocument() { return xmlDoc; }	
};

inline void deleteSimulator() {
	Simulator::deleteSimulator();
}

inline Simulator* getSimulator() {	
	return Simulator::getSimulator();
}

} // BaseSimulator namespace

#endif /* SIMULATOR_H_ */
