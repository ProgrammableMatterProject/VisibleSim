/*
 * @file multiRobotsBlock.h
 *
 *  Created on: 14/07/2016
 *      Author: pthalamy
 */

#ifndef MULTIROBOTSBLOCK_H_
#define MULTIROBOTSBLOCK_H_

#include <stdexcept>

#include "buildingBlock.h"
#include "multiRobotsBlockCode.h"
#include "multiRobotsGlBlock.h"
#include "lattice.h"

namespace MultiRobots {

class MultiRobotsBlockCode;

class MultiRobotsBlock : public BaseSimulator::BuildingBlock {
// public:
//     list<BlinkyBlocks::BlinkyBlocksBlock*>connected;
public:    
	MultiRobotsBlock(int bId, BlockCodeBuilder bcb);
	~MultiRobotsBlock();

	inline virtual MultiRobotsGlBlock* getGlBlock() { return (MultiRobotsGlBlock *)ptrGlBlock; };

	P2PNetworkInterface *getInterfaceDestId(int id);
	SLattice::Direction getDirection(P2PNetworkInterface*);

	void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target);
	void removeNeighbor(P2PNetworkInterface *ni);
	void stopBlock(uint64_t date, State s);
	void pauseClock(uint64_t delay, uint64_t start);

    // void BlinkyBlocksBlock::addEdge(BuildingBlock* target) {
	// 	getScheduler()->scheduleLock(new AddEdgeEvent(getScheduler()->now(), this, target->blockId));
	// }

};

std::ostream& operator<<(std::ostream &stream, MultiRobotsBlock const& bb);

}

#endif /* MULTIROBOTSBLOCK_H_ */
