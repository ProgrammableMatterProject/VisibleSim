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
#include <map>

#include "trace.h"
#include "target.h"
#include "TinyXML/tinyxml.h"

class Event;
typedef std::shared_ptr<Event> EventPtr;
class Message;
class P2PNetworkInterface;

namespace BaseSimulator {

class Scheduler;
class Lattice;
class BuildingBlock;
class BlockCode;

typedef std::function<void (BlockCode*,std::shared_ptr<Message>,P2PNetworkInterface*)> eventFunc;

/**
 * @brief A distributed user program, will be executed by each module 
 */ 
class BlockCode {
public:
    BuildingBlock *hostBlock; 	//!< The block to which this instance of the user program belongs 
    uint64_t availabilityDate = 0; //!< If the host is busy, the date at which it will be available
    std::multimap<int,eventFunc> eventFuncMap; //!< 
    Scheduler *scheduler; //!< pointer to the single instance of scheduler of the simulation
    Lattice *lattice;  //!< pointer to the single instance of lattice of the simulation
    ConsoleStream console;  //!< pointer to the single instance of ConsoleStream of the simulation
    static Target *target; //!< pointer shared by all blockCodes to the current target configuration    
	// static TiXmlNode* xmlTargetListNode; //!< targetList XML node from the configuration file
    
/**
 * @brief BlockCode constructor
 * @para, host The block on which this instance of the blockCode will be executed
 */ 
    BlockCode(BuildingBlock *host);    
/**
 * @brief BlockCode destructor
 */ 
    virtual ~BlockCode();
    
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
    virtual void processLocalEvent(EventPtr pev);
/**
 * @brief This function is called on startup of the blockCode, 
 it can be used to perform initial configuration of the host or this instance of the program
*/ 
    virtual void startup() = 0;
/**
 * @brief Initialization function called by startup(), used for scheduling initial events and performing additional configuration if needed.
 */ 
    virtual void init() {};

//virtual bool getAttribute(const string &att,ostringstream &sout) { sout << "no debugging"; return false; };
    virtual void addDebugAttributes(Scheduler *scheduler){};

    virtual void onTap(int face);
    
    void addMessageEventFunc(int type,eventFunc);
    int sendMessageToAllNeighbors(Message*,int t0,int dt,int nexcept,...);
    int sendMessage(Message*,P2PNetworkInterface *,int t0,int dt);
    int sendMessageToAllNeighbors(const char*,Message*,int t0,int dt,int nexcept,...);
    int sendMessage(const char*,Message*,P2PNetworkInterface *,int t0,int dt);
};

} // BaseSimulator namespace

#endif /* BLOCKCODE_H_ */
