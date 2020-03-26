/*
 * catoms2DBlock.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "catoms2DBlock.h"
#include "buildingBlock.h"
#include "catoms2DWorld.h"
#include "catoms2DSimulator.h"

#include "rotation2DEvents.h"
#include "trace.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Catoms2D {

Catoms2DBlock::Catoms2DBlock(int bId, BlockCodeBuilder bcb)
  : BaseSimulator::BuildingBlock(bId, bcb, HLattice::MAX_NB_NEIGHBORS) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "Catoms2DBlock constructor" << endl;
#endif

    angle = 0;
    doubleRNG g = Random::getNormalDoubleRNG(getRandomUint(),CATOMS2D_MOTION_SPEED_MEAN,CATOMS2D_MOTION_SPEED_SD);
    RandomRate *speed = new RandomRate(g);
    motionEngine = new Catoms2DMotionEngine(speed);
}

Catoms2DBlock::~Catoms2DBlock() {
    OUTPUT << "Catoms2DBlock destructor " << blockId << endl;
}

// PTHY: TODO: Can be genericized in BuildingBlocks
int Catoms2DBlock::getDirection(P2PNetworkInterface *p2p) const {
    if (!p2p) {
      return HLattice::Direction(0);
    }

    for (int i = 0; i < HLattice::MAX_NB_NEIGHBORS; ++i) {
      if (getInterface(HLattice::Direction(i)) == p2p) {
    return HLattice::Direction(i);
      }
    }
    return HLattice::Direction(0);
}

// PTHY: TODO: Take rotation into account
Cell3DPosition Catoms2DBlock::getPosition(HLattice::Direction d) const {
    World *wrl = getWorld();
    vector<Cell3DPosition> nCells = wrl->lattice->getRelativeConnectivity(position);
    return position + nCells[d];
}

// PTHY: TODO: Can be genericized in BuildingBlocks
Cell3DPosition Catoms2DBlock::getPosition(P2PNetworkInterface *p2p) const{
    return getPosition((HLattice::Direction)getDirection(p2p));
}

std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color << "\tpos: " << bb.position;
    return stream;
}

P2PNetworkInterface *Catoms2DBlock::getInterface(HLattice::Direction d) const {
    int alpha = angle;
    int beta = d*60;
    int t = beta-alpha;

    if (t>=0){
        return P2PNetworkInterfaces[(t/60)%6];
    } else {
        return P2PNetworkInterfaces[((360+t)/60)%6];
    }
}

bool Catoms2DBlock::hasANeighbor(HLattice::Direction n, bool groundIsNeighbor) const {
    return hasANeighbor(getInterface(n),groundIsNeighbor);
}

bool Catoms2DBlock::hasANeighbor(P2PNetworkInterface *p2p, bool groundIsNeighbor) const {
    Cell3DPosition p = getPosition(p2p);
    if(p2p->connectedInterface) {
        return true;
    } else if (groundIsNeighbor && (p[2]<0)) {
        return true;
    }
    return false;
}

int Catoms2DBlock::nbNeighbors(bool groundIsNeighbor) const {
    int cnt = 0;
    for (int i = 0; i < HLattice::MAX_NB_NEIGHBORS; i++) {
        if (hasANeighbor((HLattice::Direction)i, groundIsNeighbor)) {
            cnt++;
        }
    }
    return cnt;
}

int Catoms2DBlock::nbConsecutiveNeighbors(bool groundIsNeighbor) const {
    int empty = -1;
    int m = 0;
    int cnt = 0;
    for(int i = 0; i < HLattice::MAX_NB_NEIGHBORS; i++) {
        if (!hasANeighbor((HLattice::Direction)i, groundIsNeighbor)) {
            empty = i;
            break;
        }
    }

    if (empty == -1) {
        return HLattice::MAX_NB_NEIGHBORS;
    }

    for( int i = 0; i < HLattice::MAX_NB_NEIGHBORS; i++) {
        int j = (empty+i)%HLattice::MAX_NB_NEIGHBORS;
        if (hasANeighbor((HLattice::Direction)j, groundIsNeighbor)) {
            cnt++;
        } else {
            m = max(m,cnt);
            cnt = 0;
        }
    }
    m = max(m,cnt);
    return m;
}

int Catoms2DBlock::nbConsecutiveEmptyFaces(bool groundIsNeighbor) const {
    int notEmpty = -1;
    int m = 0;
    int cnt = 0;
    for(int i = 0; i < HLattice::MAX_NB_NEIGHBORS; i++) {
        if (hasANeighbor((HLattice::Direction)i, groundIsNeighbor)) {
            notEmpty = i;
            break;
        }
    }

    if (notEmpty == -1) {
        return 0;
    }

    for( int i = 0; i < HLattice::MAX_NB_NEIGHBORS; i++) {
        int j = (notEmpty+i)%HLattice::MAX_NB_NEIGHBORS;
        if (!hasANeighbor((HLattice::Direction)j, groundIsNeighbor)) {
            cnt++;
        } else {
            m = max(m,cnt);
            cnt = 0;
        }
    }
    m = max(m,cnt);
    return m;
}


bool Catoms2DBlock::isBlocked() const {
    int n = nbNeighbors(true);
    int nc = nbConsecutiveNeighbors(true);
    return (!((n == nc) && (nc <= 3)));
}

P2PNetworkInterface* Catoms2DBlock::getNextInterface(RelativeDirection::Direction dir,
                                                     P2PNetworkInterface *p2p,
                                                     bool connected) const {
    P2PNetworkInterface *next = NULL;
    int d = getDirection(p2p);

    do {
        if (dir == RelativeDirection::CW) {
            if (d == HLattice::Right) {
                d= HLattice::BottomRight;
            } else {
                d--;
            }
        } else {
            d = (d+1)%HLattice::MAX_NB_NEIGHBORS;
        }
        next = getInterface((HLattice::Direction)d);
        if (!connected)  {
            break;
        }

    } while((next->connectedInterface == NULL) && (next != p2p));
    return next;
}



int Catoms2DBlock::getCCWMovePivotId() const {
    for (int j = 0; j < 6; j++) {
        P2PNetworkInterface *p2pPivot = getInterface((HLattice::Direction)j);

        if (p2pPivot->connectedInterface) {
            // cout << j << ".isConnected: ";

            bool res = true;
            for (int i = 1; i < 4; i++) {
                int index = ((j - i)%6 + 6)%6;
                P2PNetworkInterface *p2pIf = getInterface((HLattice::Direction)index);
                if (p2pIf->connectedInterface) {
                    // cout << index << ".false ";
                    res = false;
                } else {
                    // cout << index << ".true ";
                }
            }

            // cout << endl;

            if (res) {
                Catoms2DBlock *piv = static_cast<Catoms2DBlock*>(p2pPivot->connectedInterface->hostBlock);
                int idDestIf = ((piv->getDirection(p2pPivot->connectedInterface) + 1)%6 + 6)%6;
                Cell3DPosition dest  = piv->getPosition((HLattice::Direction)idDestIf);
                if (getWorld()->lattice->isInGrid(dest)) {
                    // cout << "Block " << position << " wants to CCW MOVEP: " << dest << endl;
                    return p2pPivot->getConnectedBlockId();
                } else {
                    // cout << "CCW Destination " << dest << " not in grid" << endl;
                }
            }
        }
    }

    return -1;
}

int Catoms2DBlock::getCWMovePivotId() const {
    for (int j = 5; j >= 0; j--) {
        P2PNetworkInterface *p2pPivot = getInterface((HLattice::Direction)j);

        if (p2pPivot->connectedInterface) {
            // cout << j << ".isConnected: ";

            bool res = true;
            for (int i = 1; i < 4; i++) {
                int index = ((j + i)%6 + 6)%6;
                P2PNetworkInterface *p2pIf = getInterface((HLattice::Direction)index);
                if (p2pIf->connectedInterface) {
                    // cout << index << ".false ";
                    res = false;
                } else {
                    // cout << index << ".true ";
                }
            }

            if (res) {
                Catoms2DBlock *piv = static_cast<Catoms2DBlock*>(p2pPivot->connectedInterface->hostBlock);
                int idDestIf = ((piv->getDirection(p2pPivot->connectedInterface) - 1)%6 + 6)%6;
                Cell3DPosition dest  = piv->getPosition((HLattice::Direction)idDestIf);
                if (getWorld()->lattice->isInGrid(dest)) {
                    // cout << "Block " << position << " wants to CW MOVEP: " << dest << endl;
                    return p2pPivot->getConnectedBlockId();
                } else {
                    // cout << "CW Destination " << dest << " not in grid" << endl;
                }
            }
        }
    }

    return -1;
}



// Motion
bool Catoms2DBlock::canMove(const Rotation2DMove &m) const {
    // physical moving condition
    // pivot is a neighbor (physically connected)
    // move CW around i connector: i+1, i+2 and i+3 should be free
    // move CCW around i connector: i-1, i-2 and i-3 should be free

    RelativeDirection::Direction direction = m.getDirection();
    Catoms2DBlock *pivot = m.getPivot();

    if ((direction != RelativeDirection::CW) &&
        (direction != RelativeDirection::CCW)) {
        cerr << "undefined move direction" << endl;
        return false;
    }

    P2PNetworkInterface *p2p = getP2PNetworkInterfaceByBlockRef(pivot);

    if (p2p == NULL) {
        cerr << "undefined move interface" << endl;
        return false;
    }

    int p2pDirection = getDirection(p2p);

    bool res = true;

    for (int i = 0; i < 3; i++) {
        if (direction == RelativeDirection::CW) {
            if (p2pDirection == HLattice::BottomRight) {
                p2pDirection = HLattice::Right;
            } else {
                p2pDirection++;
            }
        } else if (direction == RelativeDirection::CCW) {
            if (p2pDirection == HLattice::Right) {
                p2pDirection = HLattice::BottomRight;
            } else {
                p2pDirection--;
            }
        }

        Cell3DPosition p = getPosition((HLattice::Direction)p2pDirection);
        if (!getWorld()->lattice->isFree(p)) {
            //cout << "somebody is connected there" << endl;
            res = false;
        }
    }

    return res;
}

void Catoms2DBlock::startMove(Catoms2DRotationMove &m, Time t) {
  getScheduler()->schedule(new Catoms2DRotationStartEvent(t,this,m));
}

void Catoms2DBlock::startMove(Catoms2DRotationMove &m) {
  startMove(m,getScheduler()->now());
}

void Catoms2DBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
getScheduler()->schedule(
    new AddNeighborEvent(getScheduler()->now(), this,
                         getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
}

void Catoms2DBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
        new RemoveNeighborEvent(getScheduler()->now(), this,
                                getWorld()->lattice->getOppositeDirection(getDirection(ni))));
}

bool Catoms2DBlock::canRotate(RelativeDirection::Direction d) const {
    int pivotId = d == RelativeDirection::Direction::CW ?
        getCWMovePivotId() : getCCWMovePivotId();

    if (pivotId == -1) return false;

    Catoms2DBlock *piv = static_cast<Catoms2DBlock*>(getWorld()->getBlockById(pivotId));
    const Rotation2DMove& rot = Rotation2DMove(piv, d);
    return canMove(rot);
}

void Catoms2DBlock::rotate(RelativeDirection::Direction d, Time t) {
    if (t == 0) t = getScheduler()->now();

    int pivotId = d == RelativeDirection::Direction::CW ?
        getCWMovePivotId() : getCCWMovePivotId();

    if (pivotId != -1) {
        Catoms2DBlock *piv = static_cast<Catoms2DBlock*>(getWorld()->getBlockById(pivotId));
        getScheduler()->schedule(new Rotation2DStartEvent(t, this, Rotation2DMove(piv, d)));
    } else {
        cerr << "#" << blockId << " cannot rotate: no pivot available" << endl;
    }
}

}
