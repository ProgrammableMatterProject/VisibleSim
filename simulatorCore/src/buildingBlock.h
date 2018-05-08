/**
 *  @file buildingBlock.h
 *  @date 22 mars 2013
 *  @author: dom
 *  @brief Defines a an abstract single module of the simulated ensemble
 */

#ifndef BUILDINGBLOCK_H_
#define BUILDINGBLOCK_H_

#include <list>
#include <random>
#include <atomic>
#include <memory>

#include "tDefs.h"
#include "glBlock.h"
#include "blockCode.h"
#include "clock.h"
#include "cell3DPosition.h"
#include "statsIndividual.h"
#include "random.h"

class Event;
typedef std::shared_ptr<Event> EventPtr;

using namespace std;

class P2PNetworkInterface;

namespace BaseSimulator {

class BlockCode;
class BuildingBlock;
class Clock;

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
	//!< State of a BuildingBlock. The block is considered alive if its state is >= 2
	enum State {STOPPED = 0, REMOVED = 1, ALIVE = 2, COMPUTING = 3};
private:
	/** 
	 * \brief state of the block, with atomic access
	 */
	std::atomic<State> state;
protected:
	static bID nextId;
	static bool userConfigHasBeenParsed; //!< Indicates if the user parsing as already been performed by blockCode->parseUserElements. Used to ensure that user configuration is parsed only once.
	
	vector<P2PNetworkInterface*> P2PNetworkInterfaces; //!< Vector of size equal to the number of interfaces of the block, contains pointers to the block's interfaces

	list<EventPtr> localEventsList; //!< List of local events scheduled for this block
public:
    bID blockId; //!< id of the block
	uintRNG generator; //!< random number generator
	BlockCode *blockCode; //!< blockcode program executed by the block
	Clock *clock; //!< internal clock of the block
	Color color; //!< color of the block
	Cell3DPosition position; //!< position of the block in the grid of cells;
	bool isMaster; //!< indicates is the block is a master block
	GlBlock *ptrGlBlock; //!< ptr to the GL object corresponding to this block
	BlockCodeBuilder buildNewBlockCode; //!< function ptr to the block's blockCodeBuilder
	utils::StatsIndividual *stats = NULL; //!< Module stats collected during the simulation
	/**
	 * @brief BuildingBlock constructor
	 * @param bId : the block id of the block to create
	 * @param bcb : function pointer to the getter for the block's CodeBlock
	 * @param nbInterfaces : number of initial interfaces of the block (Necessary for MeldInterpretVM init)
	 * @param seed : seed used to create the block random generator
	 */
	BuildingBlock(int bId, BlockCodeBuilder bcb, int nbInterfaces);
	
	/**
	 * @brief BuildingBlock destructor
	 */
	virtual ~BuildingBlock();  

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
	 * @brief Getter for a specific P2PNetworkInterface, identified by its direction
	 * For all blocks that cannot rotate, the direction will always be equal to the index in the P2PNetworkInterfaces array.
	 * However, for all rotation-enabled blocks, we have to consider the angle as an offset to the index.
	 * @param direction : Lattice::Direction to which the interface is pointing
	 * @return A pointer to the P2PNeighborInterface at direction of the block */
    // virtual P2PNetworkInterface *getInterfaceForDirection(int direction) { return P2PNetworkInterfaces[i]; }
	/**
	 * @brief Returns the interface from this block that is connected to block of id destBlockId
	 * @param destBlockId : id of the block connected to the interface we are looking for
	 * @return a pointer to the interface connected to the requested block, or NULL
	 */
	P2PNetworkInterface *getP2PNetworkInterfaceByDestBlockId(bID destBlockId);
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
	virtual void stop(Time date, State s) {};
	/**
	 * @brief Returns the direction (defined in lattice.h) corresponding to the interface p2p
	 * @param p2p interface to consider
	 * @return direction on which p2p is
	 */	
	virtual int getDirection(P2PNetworkInterface *p2p) = 0;
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
	/**
	 * @brief Return a random unsigned int (ruint) using the generator field
	 * @return random ruint
	 */ 
	ruint getRandomUint();
	/**
	 * @brief Schedules a tap event at a given date for this blocks
	 * When triggered from the simulation menu,
	 *  can be used as an interactive event for debug on all catom types
	 * @param date : date of the tap event
	 * @param face : id of the tapped face
	 */   	
	void tap(Time date, int face);
	/**
	 * @brief Set the internal clock to the clock in parameter
	 * @param c clock which the internal clock will be set
	 */
	void setClock(Clock *c);
	/**
	 * @brief Returns the current local time of the block according to its internal clock
	 * @return current local time of the block according to its internal clock
	 */   	
	Time getLocalTime();
	/**
	 * @brief Returns the local time of the block according to its internal clock
	 * @para simTime simulation time for which this function returns the block clock local time
	 * @return local time of the block according to its internal clock
	 */ 
	Time getLocalTime(Time simTime);
	/**
	 * @brief Converts the block's local time into the global time of the simulation and returns it
	 * @return global time corresponding to the local time in parameter
	 */   	
	Time getSimulationTime(Time localTime);
	
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
	void setBlinkMode(bool b) { ptrGlBlock->isHighlighted=b; };
};

} // BaseSimulator namespace

#endif /* BUILDINGBLOCK_H_ */
