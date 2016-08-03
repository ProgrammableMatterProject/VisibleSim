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
	enum IDScheme {ORDERED = 0, MANUAL, RANDOM};

	static bool regrTesting;			//!< Indicates if this simulation instance is performing regression testing
	//!< (causes configuration export before simulator termination) 

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
	//<! @brief Exception thrown if an error as occured during parsing
    struct ParsingException : std::exception {
        const char* what() const noexcept {
            return "An error occured during configuration file parsing\n";
        }
    };

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
	vector<int> IDPool; //!< Vector whose size is the number of blocks in the configuration and that contains blockIds to be assigned to the block, in their order of appearance in the configuration file (by default: {1,2,3,...,n})
	IDScheme ids = ORDERED; //!< Determines what module ID distribution scheme the simulator is using. ORDERED by default
	
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

	/*!
	 *  @brief Examines the configuration file's blockList attribute and determine the ID distribution scheme to be used
	 *  @return the enum IDScheme value corresponding to the scheme to be used. ORDERED, if none specified in the configuration.
	 *  @attention The xmlBlockListNode attribute has to be initialized before calling this function.
	 */
	IDScheme determineIDScheme();
		
	/*!
	 *  @brief Initialize the pool of id according to the ID assignment model specified in configuration file
	 *  If it does not exist, initialize IDPool with with contiguous integers up to N {1,2,3,...,N}
	 *  @attention The xmlBlockListNode attribute has to be initialized before calling this function.
	 */
	void initializeIDPool();

	/*!
	 *  @brief Initializes IDPool with n ID distanced by step and shuffled
	 *  @param n the number of IDs to generate
	 *  @param seed the random seed used to configure the random number generator. If seed = -1, a random seed is used instead.
	 *  @param step the distance between two consecutive numbers. e.g. If n = 4 and step = 2 then IDPool = {1, 3, 5, 7}
	 *  @attention The user has to ensure that the generation won't cause any overflow
	 */
	void generateRandomIDs(const int n, const int seed, const int step);
	
	/*!
	 *  @brief Parses the configuration file's blockList attribute for the random seed attribute
	 *  @return The integer seed specified in the configuration file, or -1 if unspecified
	 *  @throw ParsingException in case the seed is not a valid integer number
	 */
	int parseRandomSeed();

	/*!
	 *  @brief Parses the configuration file's blockList attribute for the random step attribute
	 *  @return The integer step specified in the configuration file, or 1 if unspecified
	 *  @throw ParsingException in case the step is not a valid integer number
	 */
	int parseRandomStep();
	
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
	
	/*! 
	 *  @brief Parses the configuration for obstacles information 
	 *
	 */
	void parseObstacles();

	//<! @brief Parses the configuration for target information, and instantiate them
	void parseTarget();

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

	/*! 
	 *  @brief Parses the config file for any required additional block attribute, and add it to the world
	 *
	 *  @param blockElt The current block XML element for parsing additional attributes
	 *  @param master id of the block to add
	 *  @param buildingBlockCodeBuildingFunction function pointer to the user blockCode
	 *  @param pos Position of the block to add
	 *  @param master True if the block is a master block, false otherwise
	 *
	 */
	virtual void loadBlock(TiXmlElement *blockElt, int blockId, BlockCodeBuilder bcb,
						   const Cell3DPosition &pos, const Color &color, bool master) {};
   
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

} // BaseSimulator namespace

#endif /* SIMULATOR_H_ */
