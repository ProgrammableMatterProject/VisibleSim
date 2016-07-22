/*
 * blinkyBlocksBlock.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef BLINKYBLOCKSBLOCK_H_
#define BLINKYBLOCKSBLOCK_H_

#include <stdexcept>

#include "buildingBlock.h"
#include "color.h"
#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksGlBlock.h"
#include "lattice.h"

namespace BlinkyBlocks {

class BlinkyBlocksBlockCode;

class BlinkyBlocksBlock : public BaseSimulator::BuildingBlock {
public:    
	BlinkyBlocksBlock(int bId, BlockCodeBuilder bcb);
	~BlinkyBlocksBlock();

	inline virtual BlinkyBlocksGlBlock* getGlBlock() { return (BlinkyBlocksGlBlock *)ptrGlBlock; };

	P2PNetworkInterface *getInterfaceDestId(int id);
	int getDirection(P2PNetworkInterface*);

	/* schedule the appropriate event for this action */
	/* void tap(uint64_t date); Now a generic event in buildingBlock.cpp */
	void accel(uint64_t date, int x, int y, int z);
	void shake(uint64_t date, int f);

	void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target);
	void removeNeighbor(P2PNetworkInterface *ni);
	void stopBlock(uint64_t date, State s);
	void pauseClock(uint64_t delay, uint64_t start);
};

std::ostream& operator<<(std::ostream &stream, BlinkyBlocksBlock const& bb);

}

#endif /* BLINKYBLOCKSBLOCK_H_ */
