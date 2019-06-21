/*
 * catoms2DBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DBLOCK_H_
#define CATOMS2DBLOCK_H_

#include <stdexcept>

#include "buildingBlock.h"
#include "catoms2DBlockCode.h"
#include "catoms2DGlBlock.h"
#include "lattice.h"
#include "catoms2DMotionEngine.h"

class Catoms2DRotationMove;

namespace Catoms2D {

class Catoms2DBlockCode;

class RelativeDirection {
 public:
  enum Direction {CW = -1, CCW =1};
  static inline Direction getOpposite(Direction d) { return (Direction) ((-1)*d); }
};

class Catoms2DBlock : public BaseSimulator::BuildingBlock {
protected:

public:
    int angle;
    Catoms2DMotionEngine *motionEngine;

    Catoms2DBlock(int bId, BlockCodeBuilder bcb);
    ~Catoms2DBlock();

    inline void setGlBlock(Catoms2DGlBlock*ptr) { ptrGlBlock=ptr;};
    P2PNetworkInterface *getInterface(HLattice::Direction d) const;
    inline P2PNetworkInterface *getInterface(int d) const {
        return P2PNetworkInterfaces[(HLattice::Direction)d];
    }

    Cell3DPosition getPosition(HLattice::Direction d) const;
    Cell3DPosition getPosition(P2PNetworkInterface *p2p) const;

    int getDirection(P2PNetworkInterface* p2p) const override;
    int nbNeighbors(bool groundIsNeighbor = false) const;
    int nbConsecutiveNeighbors(bool groundIsNeighbor = false) const;
    int nbConsecutiveEmptyFaces(bool groundIsNeighbor = false) const;
    bool hasANeighbor(HLattice::Direction n, bool groundIsNeighbor = false) const;
    bool hasANeighbor(P2PNetworkInterface *p2p, bool groundIsNeighbor = false) const;

    //inline direction_t getOpposite(direction_t d) { return (direction_t) (d * (-1));}
    P2PNetworkInterface* getNextInterface(RelativeDirection::Direction dir,
                                          P2PNetworkInterface *p2p,
                                          bool connected = false) const;

    // Motion
    bool isBlocked() const;
    bool canMove(Catoms2DRotationMove &m) const;
    int getCCWMovePivotId() const;
    int getCWMovePivotId() const;
    void startMove(Catoms2DRotationMove &m, Time t);
    void startMove(Catoms2DRotationMove &m);

    // MeldInterpreter
    /**
     * @copydoc BuildingBlock::addNeighbor
     */
    virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) override;
    /**
     * @copydoc BuildingBlock::removeNeighbor
     */
    virtual void removeNeighbor(P2PNetworkInterface *ni) override;

};

std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb);

}

#endif /* CATOMS2DBLOCK_H_ */
