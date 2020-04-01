/*
 * smartBlocksBlock.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSBLOCK_H_
#define SMARTBLOCKSBLOCK_H_

#include "base/buildingBlock.h"
#include "robots/smartBlocks/smartBlocksBlockCode.h"
#include "robots/smartBlocks/smartBlocksGlBlock.h"
#include "comm/network.h"
#include "grid/lattice.h"

namespace SmartBlocks {

class SmartBlocksBlockCode;

class SmartBlocksBlock : public BaseSimulator::BuildingBlock {
public:
    bool wellPlaced,_isBorder,_isTrain,_isSingle;

    SmartBlocksBlock(int bId, BlockCodeBuilder bcb);
    ~SmartBlocksBlock();
    inline void setDisplayedValue(int n) { static_cast<SmartBlocksGlBlock*>(ptrGlBlock)->setDisplayedValue(n); };
    inline P2PNetworkInterface *getInterface(SLattice::Direction d) const { return P2PNetworkInterfaces[d]; }
    P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos) const;
    P2PNetworkInterface *getP2PNetworkInterfaceByDestBlockId(bID id) const;

    Cell3DPosition getPosition(SLattice::Direction d) const;
    int getDirection( P2PNetworkInterface*) const override;
    inline void getGridPosition(int &x,int &y) const {
        x = int(position[0]); y=int(position[1]);
    };

    /**
     * @copydoc BuildingBlock::addNeighbor
     */
    virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) override;
    /**
     * @copydoc BuildingBlock::removeNeighbor
     */
    virtual void removeNeighbor(P2PNetworkInterface *ni) override;
};

}

#endif /* SMARTBLOCKSBLOCK_H_ */
