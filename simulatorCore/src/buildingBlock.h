/*
 * buildingBlock.h
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#ifndef BUILDINGBLOCK_H_
#define BUILDINGBLOCK_H_

#include <boost/shared_ptr.hpp>
#include <boost/random.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>

#include <list>

#include "glBlock.h"
#include "blockCode.h"
#include "clock.h"
#include "Cell3DPosition.h"

class Event;
typedef boost::shared_ptr<Event> EventPtr;

using namespace std;

class P2PNetworkInterface;

namespace BaseSimulator {

class BlockCode;
class Clock;

//===========================================================================================================
//
//          BuildingBlock  (class)
//
//===========================================================================================================


class BuildingBlock {
protected:
	static int nextId;
	//static std::tr1::unordered_set<BuildingBlock*> buildingBlocksSet;

	int P2PNetworkInterfaceNextLocalId;
	list<P2PNetworkInterface*> P2PNetworkInterfaceList;

	list<EventPtr> localEventsList;

	/* Graphical interface and Scheduler can access to the state of
	 * block. The scheduler only read the state, usually read on int
	 * can be considered as atomic but it depends on the platform
	 * architecture. We can also use atomic type (c++11, or boost 1.53)
	 */
	boost::interprocess::interprocess_mutex mutex_state;

public:
	// alive state must be associated to a number >= 2
	enum State {STOPPED = 0, REMOVED = 1, ALIVE = 2, COMPUTING = 3};
	State state; //!< state of the block
	int blockId; //!< id of the block
	// PTHY: MEMO
	boost::rand48 generator; //!< pseudo-random number generator for BlinkyBlocks determinism
	BlockCode *blockCode; //!< blockcode program executed by the block
	Clock *clock; //!< internal clock of the block
	Color color; //!< color of the block
	Cell3DPosition position; //!< position of the block in the grid of cells;
	bool isMaster; //!< indicates is the block is a master block
	GlBlock *ptrGlBlock; //!< ptr to the GL object corresponding to this block

	BuildingBlock(int bId);
	virtual ~BuildingBlock();

	unsigned int getNextP2PInterfaceLocalId();

	list<P2PNetworkInterface*>& getP2PNetworkInterfaceList() {return P2PNetworkInterfaceList;}
	P2PNetworkInterface *getP2PNetworkInterfaceByDestBlockId(int destBlockId);
	bool addP2PNetworkInterfaceAndConnectTo(BuildingBlock *destBlock);
	bool addP2PNetworkInterfaceAndConnectTo(int destBlockId);
	P2PNetworkInterface *getP2PNetworkInterfaceByBlockRef(BuildingBlock *destBlock);

	void scheduleLocalEvent(EventPtr pev);
	void processLocalEvent();

	virtual void updateGlData() {};
	inline virtual GlBlock* getGlBlock() { return ptrGlBlock; };
	inline void setGlBlock(GlBlock*ptr) { ptrGlBlock=ptr;};

	void setColor(const Color &);
	void setColor(int idColor);

	void setPosition(const Cell3DPosition &p);
	void setPosition(const Vector3D &p);

	/**
	 * Returns a Vector3D corresponding to the Cell3DPosition of the current block.
	 *
	 * @return the position of the block represented as a double Vector3D
	 */
	inline Vector3D getPositionVector() { return Vector3D(position[0], position[1], position[2]);};
	// void setColor(int num);

	virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {};
	virtual void removeNeighbor(P2PNetworkInterface *ni) {};
	virtual void stop(uint64_t date, State s) {};
	/* No guarantee that state value will remain the same, it just avoids
	 * date race condition.
	 */
	inline void lock() { mutex_state.lock(); }
	inline void unlock() { mutex_state.unlock(); }
	inline State getState() { lock(); State s = state; unlock(); return s; }
	inline void setState(State s) { lock(); state = s; unlock();}

	/* For Blinky Block determinism version */
	int getNextRandomNumber();

	/* When triggered from the simulation menu,
	   can be used as an interactive event for debug on all catom types */
	void tap(uint64_t date);

	uint64_t getTime();
	uint64_t getSchedulerTimeForLocalTime(uint64_t localTime);

};

} // BaseSimulator namespace

#endif /* BUILDINGBLOCK_H_ */
