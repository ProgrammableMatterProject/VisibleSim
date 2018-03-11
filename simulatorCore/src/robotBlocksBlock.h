/*
 * robotBlocksBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOTBLOCKSBLOCK_H_
#define ROBOTBLOCKSBLOCK_H_

#include <stdexcept>

#include "buildingBlock.h"
#include "robotBlocksBlockCode.h"
#include "robotBlocksGlBlock.h"
#include "lattice.h"

namespace RobotBlocks {

class RobotBlocksBlockCode;

class RobotBlocksBlock : public BaseSimulator::BuildingBlock {
protected:
public:
    RobotBlocksBlock(int bId, BlockCodeBuilder bcb);
    ~RobotBlocksBlock();

    inline RobotBlocksGlBlock* getGlBlock() { return (RobotBlocksGlBlock*)ptrGlBlock; };
    inline void setGlBlock(RobotBlocksGlBlock*ptr) { ptrGlBlock=ptr;};
    void setPrevNext(int,int);
    void setPrevNext(const P2PNetworkInterface *prev,const P2PNetworkInterface *next);
    P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos);
    inline P2PNetworkInterface *getInterface(SCLattice::Direction d) { return P2PNetworkInterfaces[d]; }
	/**
	 * @copydoc BuildingBlock::addNeighbor
	 */
	virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target);
	/**
	 * @copydoc BuildingBlock::removeNeighbor
	 */
	virtual void removeNeighbor(P2PNetworkInterface *ni);
    int getDirection(P2PNetworkInterface*);
};

std::ostream& operator<<(std::ostream &stream, RobotBlocksBlock const& bb);

}

#endif /* ROBOTBLOCKSBLOCK_H_ */
