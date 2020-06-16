/*
 * SmartBlocksBlock.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include "smartBlocksBlock.h"
#include "smartBlocksWorld.h"
#include "../../motion/translationEvents.h"
#include "../../replay/replayExporter.h"

using namespace std;

namespace SmartBlocks {

SmartBlocksBlock::SmartBlocksBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb, SLattice::MAX_NB_NEIGHBORS) {
    orientationCode=0;
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "SmartBlocksBlock #" << bId << " constructor" << endl;
#endif
}

SmartBlocksBlock::~SmartBlocksBlock() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "SmartBlocksBlock #" << blockId << " destructor" << endl;
#endif
}

P2PNetworkInterface *SmartBlocksBlock::getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos) const {
    if (pos[0]==-1) return P2PNetworkInterfaces[SLattice::West];
    else if (pos[0]==1) return P2PNetworkInterfaces[SLattice::East];
    else if (pos[1]==-1) return P2PNetworkInterfaces[SLattice::South];
    else if (pos[1]==1) return P2PNetworkInterfaces[SLattice::North];

    return NULL;
}

int SmartBlocksBlock::getDirection(P2PNetworkInterface *given_interface) const {
    /*if( !given_interface) {
      return SLattice::Direction(0);
      }*/
    for( int i( SLattice::North); i <= SLattice::West; ++i) {
        P2PNetworkInterface* p2p = P2PNetworkInterfaces[ i];
        if( p2p == given_interface) {
            return SLattice::Direction(i);
        }
    }
    assert(0);			// should never get here
}

Cell3DPosition SmartBlocksBlock::getPosition(SLattice::Direction d) const {
    Cell3DPosition p = position;

    switch (d) {
    case SLattice::North :
        p.pt[1]++;
        break;
    case SLattice::East :
        p.pt[0]--;
        break;
    case SLattice::South :
        p.pt[1]--;
        break;
    case SLattice::West :
        p.pt[0]++;
        break;
    default :
        cerr << "error: getPosition: Undefined position " << d << endl;
        exit(EXIT_FAILURE);
    }


    return p;
}

P2PNetworkInterface *SmartBlocksBlock::getP2PNetworkInterfaceByDestBlockId(bID id) const {
    int i=0;
    while (i<4 && (P2PNetworkInterfaces[i]->connectedInterface == NULL
                   || P2PNetworkInterfaces[i]->connectedInterface->hostBlock->blockId != id)) {
        i++;
    }
    return (i<4?P2PNetworkInterfaces[i]:NULL);
}


void SmartBlocksBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
        new AddNeighborEvent(getScheduler()->now(), this,
                             getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
}

void SmartBlocksBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
        new RemoveNeighborEvent(getScheduler()->now(), this,
                                getWorld()->lattice->getOppositeDirection(getDirection(ni))));
}

bool SmartBlocksBlock::canMoveTo(const Cell3DPosition& dest) const {
    Lattice *lattice = getWorld()->lattice;

    return lattice->isInGrid(dest)
        and lattice->cellsAreAdjacent(position, dest)
        and lattice->isFree(dest);
}

bool SmartBlocksBlock::moveTo(const Cell3DPosition& dest) {
    if (not canMoveTo(dest)) return false;

    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeMotion(getScheduler()->now(), blockId,102000,dest); // BPi : duration is currently fixed due to translation event implementation

    getScheduler()->schedule(new TranslationStartEvent(getScheduler()->now(), this, dest));
    return true;
}

void SmartBlocksBlock::setDisplayedValue(uint16_t n) {
    static_cast<SmartBlocksGlBlock*>(ptrGlBlock)->setDisplayedValue(n);
    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeDisplayUpdate(getScheduler()->now(), blockId, n);
}

}
