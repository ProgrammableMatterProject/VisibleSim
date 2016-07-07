/*
 * buildingBlock.h
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#ifndef BUILDINGBLOCK_H_
#define BUILDINGBLOCK_H_

#include <list>
#include <random>
#include <atomic>
#include <memory>

#include "glBlock.h"
#include "blockCode.h"
#include "clock.h"
#include "cell3DPosition.h"
#include "capabilities.h"

class Event;
typedef std::shared_ptr<Event> EventPtr;

using namespace std;

class P2PNetworkInterface;

namespace BaseSimulator {

class BlockCode;
class Clock;
class BuildingBlock;

typedef BlockCode *(*BlockCodeBuilder)(BuildingBlock*);

//===========================================================================================================
//
//          BuildingBlock  (class)
//
//===========================================================================================================

/**
 * \brief Abstract parent class of any block
 */
class BuildingBlock {
public:
	//!< Enumeration of possible values for the state of a BuildingBlock. The block is considered alive if its state is >= 2
	enum State {STOPPED = 0, REMOVED = 1, ALIVE = 2, COMPUTING = 3};
private:
	/** 
	 * \brief state of the block, with atomic access
	 * Graphical interface and Scheduler can access to the state of
	 * block. The scheduler only read the state, usually read on int
	 * can be considered as atomic but it depends on the platform
	 * architecture. We can also use atomic type (c++11, or boost 1.53)
	 */
	std::atomic<State> state;
protected:
	static int nextId;

	int P2PNetworkInterfaceNextLocalId; // @todo
	vector<P2PNetworkInterface*> P2PNetworkInterfaces; //!< Vector of size equal to the number of interfaces of the block, contains pointers to the block's interfaces

	list<EventPtr> localEventsList; //!< List of local events scheduled for this block
public:
	int blockId; //!< id of the block
	std::ranlux48 generator; //!< random device to generate random numbers for BlinkyBlocks determinism
    std::uniform_int_distribution<> dis; //!< random int distribution based on generator
	BlockCode *blockCode; //!< blockcode program executed by the block
	Clock *clock; //!< internal clock of the block
	Color color; //!< color of the block
	Cell3DPosition position; //!< position of the block in the grid of cells;
	bool isMaster; //!< indicates is the block is a master block
	GlBlock *ptrGlBlock; //!< ptr to the GL object corresponding to this block
	BlockCodeBuilder buildNewBlockCode; //!< function ptr to the block's blockCodeBuilder
	
	/**
	 * @brief BuildingBlock constructor
	 * @param bId : the block id of the block to create
	 * @param bcb : function pointer to the getter for the block's CodeBlock
	 * @param nbInterfaces : number of initial interfaces of the block (Necessary for MeldInterpretVM init)
	 */
	BuildingBlock(int bId, BlockCodeBuilder bcb, int nbInterfaces);
    /**
	 * @brief BuildingBlock destructor
	 */
	virtual ~BuildingBlock();

	unsigned int getNextP2PInterfaceLocalId();

	/**
	 * @brief Getter for P2PNetworkInterfaces attribute
	 * @return A vector containing pointers to the block's interfaces
	 */
    vector<P2PNetworkInterface*>& getP2PNetworkInterfaces() { return P2PNetworkInterfaces; }
	/**
	 * @brief Getter for a specific P2PNetworkInterface
	 * @param i : index of interface to return
	 * @return A pointer to the P2PNeighborInterface at index i of interface vector
	 */
    P2PNetworkInterface *getInterface(int i) { return P2PNetworkInterfaces[i]; }
	/**
	 * @brief Returns the interface from this block that is connected to block of id destBlockId
	 * @param destBlockId : id of the block connected to the interface we are looking for
	 * @return a pointer to the interface connected to the requested block, or NULL
	 */
	P2PNetworkInterface *getP2PNetworkInterfaceByDestBlockId(int destBlockId);
	/**
	 * @brief Creates a new interface to this block and connects it to destBlock
	 * @param destBlock : pointer to the building block to connect to the newly created interface
	 * @return true if connection succeeded, false otherwise
	 */
	bool addP2PNetworkInterfaceAndConnectTo(BuildingBlock *destBlock);
	/**
	 * @brief Creates a new interface to this block and connects it to block of id destBlockId
	 * @param destBlockId : Id of the building block to connect to the newly created interface
	 * @return true if connection succeeded, false otherwise
	 */
	bool addP2PNetworkInterfaceAndConnectTo(int destBlockId);
	/**
	 * @brief Finds the block's interface that is connected to destBlock 
	 * @param destBlock : pointer to a connected block 
	 * @return a pointer to the interface connected to destBlock if there is one, NULL otherwise
	 */
	P2PNetworkInterface *getP2PNetworkInterfaceByBlockRef(BuildingBlock *destBlock);
	/**
	 * @brief Schedules a local event for this block to process when available
	 * @param pev : pointer to the event to schedule
	 */
	void scheduleLocalEvent(EventPtr pev);
	/**
	 * @brief Processes the first event from the event queue
	 */
	void processLocalEvent();
	/**
	 * @brief Returns the GlBlock corresponding to this BuildingBlock
	 */
	inline virtual GlBlock* getGlBlock() { return ptrGlBlock; };
	/**
	 * @brief Setter for ptrGlBlock
	 * @param ptr : a ptr to a GlBlock corresponding to this block
	 */
	inline void setGlBlock(GlBlock*ptr) { ptrGlBlock=ptr;};
	/**
	 * @brief Sets the color for this block with the referenced color parameter
	 * @param c : a reference to the new color of the block
	 */
	void setColor(const Color &);
	/**
	 * @brief Sets the color for this block with the color of id idColor
	 * @param idColor : id of the Color as defined in color.h
	 */
	void setColor(int idColor);	
	/**
	 * @brief Sets the grid position of the block
	 *
	 * @param p :  the grid position (x,y,z) of the block as a Cell3DPosition
	 */
	void setPosition(const Cell3DPosition &p);

	/**
	 * @brief Returns a Vector3D corresponding to the Cell3DPosition of the current block.
	 *
	 * @return the position of the block represented as a double Vector3D
	 */
	inline Vector3D getPositionVector() { return Vector3D(position[0], position[1], position[2]);};
	/**
	 * @brief Schedules an AddNewNeighbor event
	 * @param ni : pointer to the interface that was just connected
	 * @param target : pointer to the BuildingBlock connected to interface ni
	 */
	virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {};	
	/**
	 * @brief Schedules a RemoveNeighborEvent
	 * @param ni : pointer to the disconnected interface
	 */
	virtual void removeNeighbor(P2PNetworkInterface *ni) {};
	/**
	 * @brief Returns the number of interfaces for this block
	 * @return number of interface for this block
	 */
	inline unsigned short getNbInterfaces() {	return P2PNetworkInterfaces.size(); };
	/**
	 * @brief Schedules a stop event for this block at a given date and update its state
	 * @param date : date at which the stop event must be processed
	 * @param s : new state of the block
	 */
	virtual void stop(uint64_t date, State s) {};
	/**
	 * @brief Atomic getter for the block's state
	 * No guarantee that state value will remain the same, it just avoids
	 * date race condition.
	 * @return the state of the block
	 */
	inline State getState() { return state.load(); }
	/**
	 * @brief Atomically sets the state of the block
	 * No guarantee that state value will remain the same, it just avoids
	 * date race condition.
	 * @param s : new state of the block
	 */   	
	inline void setState(State s) { state.store(s); }
	/* For Blinky Block determinism version */
	int getNextRandomNumber();
	/**
	 * @brief Schedules a tap event at a given date for this blocks
	 * When triggered from the simulation menu,
	 *  can be used as an interactive event for debug on all catom types
	 * @param date : date of the tap event
	 */   	
	void tap(uint64_t date, bool debug = false);// PTHY: TEMPORARY! Debug event should be handled by user
	/**
	 * @brief Returns the local time of the block according to its internal clock
	 * @return local time of the block according to its internal clock
	 */   	
	uint64_t getTime();
	/**
	 * @brief Converts the block's local time into the global time of the simulation and returns it
	 * @return global time corresponding to the local time in parameter
	 */   	
	uint64_t getSchedulerTimeForLocalTime(uint64_t localTime);

	/*************************************************
	 *            MeldInterpreter Functions  
	 *************************************************/
	/**
	 * @brief Returns the id of the block connected on interface #faceNum of this block
	 * @param faceNum : id of the connected interface
	 * @return id of the block connected to interface faceNum 
	 */
	unsigned short getNeighborIDForFace(int faceNum);
	/**
	 * @brief Returns the id of the face from this block connected to block of id nId
	 * @param nId : id of the connected block 
	 * @return the id of the face connected to block nId, or -1 if the two blocks are not neighbors
	 */
	int getFaceForNeighborID(int nId);
};

} // BaseSimulator namespace

#endif /* BUILDINGBLOCK_H_ */
