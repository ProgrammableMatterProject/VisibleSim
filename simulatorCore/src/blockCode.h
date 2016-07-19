/*
 * @file blockCode.h
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#ifndef BLOCKCODE_H_
#define BLOCKCODE_H_

#include <inttypes.h>
#include <memory>

#include "TinyXML/tinyxml.h"

#include "buildingBlock.h"

class Event;
typedef std::shared_ptr<Event> EventPtr;

namespace BaseSimulator {

class Scheduler;
class BuildingBlock;

/**
 * @brief A distributed user program, will be executed by each module 
 */ 
class BlockCode {
public:
    BuildingBlock *hostBlock; 	//!< The block to which this instance of the user program belongs 
    uint64_t availabilityDate = 0; //!< If the host is busy, the date at which it will be available
    
    /**
     * @brief BlockCode constructor
     * @paran host The block on which this instance of the codeBlock will be executed
     */ 
    BlockCode(BuildingBlock *host) : hostBlock(host) {};
    /**
     * @brief BlockCode destructor
     */ 
    virtual ~BlockCode() {};
    /**
     * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from it. Has to be overriden in the child class.
     * @param config : pointer to the TiXmlDocument representing the configuration file, all information related to VisibleSim's core have already been parsed
     *
     * Called from BuildingBlock constructor, only once.
     */ 
    virtual void parseUserElements(TiXmlDocument *config) { }
    /**
     * @brief Handler for all events received by the host block
     */ 
    virtual void processLocalEvent(EventPtr pev) = 0;
    /**
     * @brief This function is called on startup of the codeBlock, 
     it can be used to perform initial configuration of the host or this instance of the program
     */ 
    virtual void startup() = 0;
    /**
     * @brief Initialization function called by startup(), used for scheduling initial events and performing additional configuration if needed.
     */ 
    virtual void init() {};

//virtual bool getAttribute(const string &att,ostringstream &sout) { sout << "no debugging"; return false; };
    virtual void addDebugAttributes(Scheduler *scheduler){};
};

} // BaseSimulator namespace

#endif /* BLOCKCODE_H_ */
