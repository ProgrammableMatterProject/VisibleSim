/*
 * slidingCubesBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef SLIDINGCUBESBLOCK_H_
#define SLIDINGCUBESBLOCK_H_

#include <stdexcept>

#include "base/buildingBlock.h"
#include "robots/slidingCubes/slidingCubesBlockCode.h"
#include "robots/slidingCubes/slidingCubesGlBlock.h"
#include "grid/lattice.h"

namespace SlidingCubes {

class SlidingCubesBlockCode;

class SlidingCubesBlock : public BaseSimulator::BuildingBlock {
protected:
public:
    SlidingCubesBlock(int bId, BlockCodeBuilder bcb);
    ~SlidingCubesBlock();

    inline SlidingCubesGlBlock* getGlBlock() const override { return (SlidingCubesGlBlock*)ptrGlBlock; };
    inline void setGlBlock(SlidingCubesGlBlock*ptr) { ptrGlBlock=ptr;};
    void setPrevNext(int,int);
    void setPrevNext(const P2PNetworkInterface *prev,const P2PNetworkInterface *next);
    P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos) const;
    inline P2PNetworkInterface *getInterface(SCLattice::Direction d) const {
        return P2PNetworkInterfaces[d];
    }

    /**
     * @copydoc BuildingBlock::addNeighbor
     */
    virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) override;
    /**
     * @copydoc BuildingBlock::removeNeighbor
     */
    virtual void removeNeighbor(P2PNetworkInterface *ni) override;
    int getDirection(P2PNetworkInterface*) const override;

    /**
     * @copydoc BuildingBlock::canMoveTo
     */
    virtual bool canMoveTo(const Cell3DPosition& dest) const override;

    /**
     * @copydoc BuildingBlock::moveTo
     */
    virtual bool moveTo(const Cell3DPosition& dest) override;
};

std::ostream& operator<<(std::ostream &stream, SlidingCubesBlock const& bb);

}

#endif /* SLIDINGCUBESBLOCK_H_ */
