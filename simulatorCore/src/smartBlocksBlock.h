/*
 * smartBlocksBlock.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSBLOCK_H_
#define SMARTBLOCKSBLOCK_H_

#include "buildingBlock.h"
#include "smartBlocksBlockCode.h"
#include "smartBlocksGlBlock.h"
#include "smartBlocksCapabilities.h"
#include "network.h"

namespace SmartBlocks {

class NeighborDirection {
public:
    enum Direction { North = 0, East, South, West};
    static int getOpposite(int d);
    static string getString(int d);
};

class SmartBlocksBlockCode;

class SmartBlocksBlock : public BaseSimulator::BuildingBlock {
public:
    bool wellPlaced,_isBorder,_isTrain,_isSingle;
    SmartBlocksBlockCode *(*buildNewBlockCode)(SmartBlocksBlock*);
    
    SmartBlocksBlock(int bId, BlockCodeBuilder bcb);
    ~SmartBlocksBlock();
    inline void setDisplayedValue(int n) { static_cast<SmartBlocksGlBlock*>(ptrGlBlock)->setDisplayedValue(n); };
    inline P2PNetworkInterface *getInterface(NeighborDirection::Direction d) { return P2PNetworkInterfaces[d]; }
    P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const PointCel &pos);
    P2PNetworkInterface *getP2PNetworkInterfaceByDestBlockId(int id);
    
    Cell3DPosition getPosition(NeighborDirection::Direction d);
    NeighborDirection::Direction getDirection( P2PNetworkInterface*);
    inline void getGridPosition(int &x,int &y) { x = int(position[0]); y=int(position[1]); };
};

}

#endif /* SMARTBLOCKSBLOCK_H_ */

