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

public:
	MultiRobotsBlock(int bId, BlockCodeBuilder bcb);
	~MultiRobotsBlock();

	inline virtual MultiRobotsGlBlock* getGlBlock() { return (MultiRobotsGlBlock *)ptrGlBlock; };

	P2PNetworkInterface *getInterfaceDestId(int id);
	BCLattice::Direction getDirection(P2PNetworkInterface*);

	void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {};
	void removeNeighbor(P2PNetworkInterface *ni) {};
	void stopBlock(uint64_t date, State s);
	void pauseClock(uint64_t delay, uint64_t start);
};

std::ostream& operator<<(std::ostream &stream, MultiRobotsBlock const& bb);

}

#endif /* MULTIROBOTSBLOCK_H_ */
