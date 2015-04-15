/*
 * buildingBlock.h
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#ifndef BUILDINGBLOCK_H_
#define BUILDINGBLOCK_H_

//#include <tr1/unordered_set>
#include <boost/shared_ptr.hpp>
#include <boost/random.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>

#include <list>

#include "glBlock.h"
#include "blockCode.h"


class Event;
typedef boost::shared_ptr<Event> EventPtr;

using namespace std;

class P2PNetworkInterface;

namespace BaseSimulator {

class BlockCode;

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
	int blockId;
	BlockCode *blockCode;
	State state;
	boost::rand48 generator;

	BuildingBlock(int bId);
	virtual ~BuildingBlock();

	unsigned int getNextP2PInterfaceLocalId();

	P2PNetworkInterface *getP2PNetworkInterfaceByDestBlockId(int destBlockId);
	bool addP2PNetworkInterfaceAndConnectTo(BuildingBlock *destBlock);
	bool addP2PNetworkInterfaceAndConnectTo(int destBlockId);
	P2PNetworkInterface *getP2PNetworkInterfaceByBlockRef(BuildingBlock *destBlock);

	void scheduleLocalEvent(EventPtr pev);
	void processLocalEvent();

	virtual void updateGlData() {};

	virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {};
	virtual void removeNeighbor(P2PNetworkInterface *ni) {};

	virtual void stop() {};
	virtual bool getAttribute(const string &att,ostringstream &sout);
	/* No guarantee that state value will remind the same, it just avoids
	 * date race condition.
	 */
	inline void lock() { mutex_state.lock(); }
	inline void unlock() { mutex_state.unlock(); }
	inline State getState() { lock(); State s = state; unlock(); return s; }
	inline void setState(State s) { lock(); state = s; unlock();}

	/* For Blinky Block determinism version */
	int getNextRandomNumber();
};

} // BaseSimulator namespace

#endif /* BUILDINGBLOCK_H_ */
