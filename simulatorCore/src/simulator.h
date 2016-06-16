/*! \file simulator.h
 *  \brief Header file for the Abstract class Simulator
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
#include "BlockCode.h"

using namespace std;

namespace BaseSimulator {

class Simulator;

extern Simulator *simulator;

/*! \class Simulator
 *  \brief Simulator is responsible for creating and configuring the core components of simulation (i.e. World, Scheduler), by parsing the configuration file and interpreting the command line args
 *
 */
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
	static Type type;			//!< Type of simulation, i.e. language of the user program

	static Simulator *simulator; //!< Static member for accessing *this* simulator
	Scheduler *scheduler;		//!< Scheduler to be instantiated and configured

	World *world; 				//!< Simulation world to be instantiated and configured
	
	TiXmlDocument *xmlDoc;		//!< TinyXMLDocument for the configuration file
	TiXmlNode* xmlWorldNode; //!< world XML node from the configuration file

	BlockCode *(*newBlockCode)(BuildingBlock*); //!< Function pointer to the target BlockCode
	
	CommandLine cmdLine;		//!< Utility member for accessing command line arguments

	/*! \fn parseWorld(int argc, char*argv[])
	 *  \brief Parses the configuration file for World information common to all blocks.
	 *
	 *  Calls the loadWorld virtual function to instantiate the right subclass of World with the parsed data.
	 *
	 *  \param argc The number of command line arguments
	 *  \param argv The command line arguments
	 *
	 */
	void parseWorld(int argc, char*argv[]);

	/*! \fn parseBlockList()
	 *  \brief Parses the configuration for block information common to all blocks
	 *
	 *  Calls the loadBlock virtual function once for every node to instantiate.
	 *
	 */	
	void parseBlockList();

	/*! \fn parseTarget(yz)
	 *  \brief Parses the configuration for target information common to all blocks
	 *
	 *  This function is only used for 2D grids.
	 *  Stores the positions of all cells to add to the target in a vector, then 
	 *   calls the loadTargetAndCapabilities virual function to configure the target 
	 *   attribute of the target world. 
	 *
	 *  \param yz The 2nd dimension: y (yz = 1) OR z (yz = 2), yz is the index of the dimension
	 *
	 *  \todo find a clever way to mention dimension to consider
	 *
	 */	
	void parseTarget(int yz);   

	/*! \fn virtual void loadWorld(int lx, int ly, int lz, int argc, char *argv[])	
	 *  \brief Calls the createWorld function from the target world subclass to instantiate it
	 *
	 *  \param lx width of the grid
	 *  \param ly depth of the grid
	 *  \param lz height of the grid
	 *  \param argc The number of command line arguments
	 *  \param argv The command line arguments
	 *
	 */		
	virtual void loadWorld(int lx, int ly, int lz, int argc, char *argv[]) = 0;

	/*! \fn virtual void loadScheduler()
	 *  \brief Calls the createScheduler function from the target scheduler subclass to instantiate it
	 *
	 */		
	virtual void loadScheduler() = 0;

	/*! \fn virtual void loadBlock(TiXmlElement *blockElt, int blockId, BlockCode *(*buildingBlockCodeBuildingFunction)(BuildingBlock*), const Cell3DPosition &pos, const Color &color, bool master)
	 *  \brief Parses the config file for any required additional block attribute, and add it to the world
	 *
	 *  \param blockElt The current block XML element for parsing additional attributes
	 *  \param master id of the block to add
	 *  \param buildingBlockCodeBuildingFunction function pointer to the user blockCode
	 *  \param pos Position of the block to add
	 *  \param master True if the block is a master block, false otherwise
	 *
	 */		
	virtual void loadBlock(TiXmlElement *blockElt, int blockId,
						   BlockCode *(*buildingBlockCodeBuildingFunction)
						   (BuildingBlock*), const Cell3DPosition &pos,
						   const Color &color, bool master) {};

	/*! \fn virtual void loadTargetAndCapabilities(vector<Cell3DPosition> targetCells)
	 *  \brief Configures the target from the simulation World with the positions in targetCells 
	 *
	 *  \param targetCells vector containing all cell positions to add to the target
	 *
	 */		
	virtual void loadTargetAndCapabilities(vector<Cell3DPosition> targetCells) {};
	
	Simulator(int argc, char *argv[]);
	virtual ~Simulator();
};
} // Simulator namespace

#endif /* SIMULATOR_H_ */
