/*
 * @file blockCode.h
 * @date 22 mars 2013
 * @author dom
 * @brief Defines a single instance of the distributed program that is executed independantly by each module
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
class HandleableMessage;
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
private:
    int sendMessageToAllNeighbors(const char*msgString, Message*msg,int t0,int dt, int nexcept, va_list args);
public:
    BuildingBlock *hostBlock; 	//!< The block to which this instance of the user program belongs 
    Time availabilityDate = 0; //!< If the host is busy, the date at which it will be available
    std::multimap<int,eventFunc> eventFuncMap; //!< container of function pointers to message handlers, indexed by message typeID
    Scheduler *scheduler; //!< pointer to the single instance of scheduler of the simulation
    Lattice *lattice;  //!< pointer to the single instance of lattice of the simulation
    ConsoleStream console;  //!< pointer to the single instance of ConsoleStream of the simulation
    static Target *target; //!< pointer shared by all blockCodes to the current target configuration    
    
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

    /**
     * @brief This function is called when a module is tapped by the user. Prints a message to the console by default. 
     Can be overloaded in the user blockCode
     * @param face face that has been tapped */
    virtual void onTap(int face);

    /**
     * @brief Loads the next target from the configuration file into the target attribute 
     *  by calling Target::loadNextTarget()
     * @return true if a target has been loaded, false otherwise (No target remaining in config file)
     */
    static bool loadNextTarget(); 
    
    /**
     * @brief Add a new message handler to the block code, for message with message type type
     * @param type ID of the message for which a handler needs to be registered
     * @param eventFunc the message handling function as a std::function */
    void addMessageEventFunc(int type,eventFunc);
    /**
     * @brief Send message to all connected interface interfaces, except those in the variadic parameters ignore list.
     *        Sending time randomly drawn as follow: tt = now + t0 + (rand * dt), where rand is either {0, 1}
     * @param msg message to be sent
     * @param t0 time of transmission
     * @param dt delta time between two transmissions
     * @param nexcept number of interfaces to ignore
     * @param ... variadic parameters: pointer to the nexcept interfaces to ignore
     * @return Number of messages effectively sent
     */
    int sendMessageToAllNeighbors(Message *msg,int t0,int dt,int nexcept,...);
    /**
     * @copydoc BlockCode::sendMessageToAllNeighbors
     * Identical to sendMessageToAllNeighbors, but prints msgString to the console when the message is sent
     * @param msgString string of the message to be printed when sent
     */
    int sendMessageToAllNeighbors(const char *msgString,Message *msg,int t0,int dt,int nexcept,...);
    /**
     * @brief Send message to interface dest at time t0 + [0,1]dt
     * @param msg message to be sent (will print the handleable message's name)
     * @param dest destination interface. 
     * @param t0 time to wait before sending
     * @param dt potential delay in sending time */
    int sendMessage(HandleableMessage *msg,P2PNetworkInterface *dest,int t0,int dt);

    /**
     * @brief Send message to interface dest at time t0 + [0,1]dt
     * @param msg message to be sent
     * @param dest destination interface. 
     * @param t0 time to wait before sending
     * @param dt potential delay in sending time */
    int sendMessage(Message *msg,P2PNetworkInterface *dest,int t0,int dt);
    /**
     * @copydoc BlockCode::sendMessage
     * @param msgString string to be printed to the console upon sending */
    int sendMessage(const char *msgString,Message *msg,P2PNetworkInterface *dest,int t0,int dt);
};

} // BaseSimulator namespace

#endif /* BLOCKCODE_H_ */
