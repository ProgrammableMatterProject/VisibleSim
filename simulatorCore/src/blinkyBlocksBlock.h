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

namespace BlinkyBlocks {

    class NeighborDirection {
    public:
	enum Direction { Bottom = 0, Back = 1, Right, Front, Left, Top };
	static int getOpposite(int d);
	static string getString(int d);
    };

    class BlinkyBlocksBlockCode;

    class BlinkyBlocksBlock : public BaseSimulator::BuildingBlock {
	P2PNetworkInterface *tabInterfaces[6];

    public:    
	BlinkyBlocksBlock(int bId, BlockCodeBuilder bcb);
	~BlinkyBlocksBlock();

	inline BlinkyBlocksGlBlock* getGlBlock() { return (BlinkyBlocksGlBlock *)ptrGlBlock; };
	inline void setGlBlock(BlinkyBlocksGlBlock*ptr) { ptrGlBlock=ptr;};
	inline P2PNetworkInterface *getInterface(NeighborDirection::Direction d) { return tabInterfaces[d]; }
	P2PNetworkInterface *getInterfaceDestId(int id);
	NeighborDirection::Direction getDirection(P2PNetworkInterface*);

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
