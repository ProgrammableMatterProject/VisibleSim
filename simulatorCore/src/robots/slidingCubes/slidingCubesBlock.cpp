/*
 * slidingCubesBlock.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>

#include "robots/slidingCubes/slidingCubesBlock.h"
#include "robots/slidingCubes/slidingCubesWorld.h"
#include "robots/slidingCubes/slidingCubesSimulator.h"
#include "grid/lattice.h"
#include "utils/trace.h"

using namespace std;

namespace SlidingCubes {

SlidingCubesBlock::SlidingCubesBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb, SCLattice::MAX_NB_NEIGHBORS) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "SlidingCubesBlock constructor" << endl;
#endif
}

SlidingCubesBlock::~SlidingCubesBlock() {
    OUTPUT << "SlidingCubesBlock destructor " << blockId << endl;
}

void SlidingCubesBlock::setPrevNext(int prev,int next) {
    getWorld()->updateGlData(this,prev,next);
}

void SlidingCubesBlock::setPrevNext(const P2PNetworkInterface *prev,const P2PNetworkInterface *next) {
    int prevId=0,nextId=0;
    if (prev) {
        SlidingCubesBlock*rb = (SlidingCubesBlock*)(prev->hostBlock);
        prevId = rb->blockId;
    }
    if (next) {
        SlidingCubesBlock*rb = (SlidingCubesBlock*)(next->hostBlock);
        nextId = rb->blockId;
    }
    //cout << (prev?prev->hostBlock->blockId:-1) << "," << (next?next->hostBlock->blockId:-1) << endl;
    getWorld()->updateGlData(this,prevId,nextId);
}

void SlidingCubesBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
        new AddNeighborEvent(getScheduler()->now(), this,
                             getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
}

void SlidingCubesBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
        new RemoveNeighborEvent(getScheduler()->now(), this,
                                getWorld()->lattice->getOppositeDirection(getDirection(ni))));
}

int SlidingCubesBlock::getDirection(P2PNetworkInterface *given_interface) const {
    if( !given_interface) {
        return SCLattice::Direction(0);
    }
    for( int i(0); i < 6; ++i) {
        if(P2PNetworkInterfaces[i] == given_interface) return SCLattice::Direction(i);
    }
    return SCLattice::Direction(0);
}

P2PNetworkInterface *SlidingCubesBlock::getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos) const {
    if (pos[0]==-1) return P2PNetworkInterfaces[SCLattice::Left];
    else if (pos[0]==1) return P2PNetworkInterfaces[SCLattice::Right];
    else if (pos[1]==-1) return P2PNetworkInterfaces[SCLattice::Front];
    else if (pos[1]==1) return P2PNetworkInterfaces[SCLattice::Back];
    else if (pos[2]==-1) return P2PNetworkInterfaces[SCLattice::Bottom];
    else if (pos[2]==1) return P2PNetworkInterfaces[SCLattice::Top];

    return NULL;
}

std::ostream& operator<<(std::ostream &stream, SlidingCubesBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
    return stream;
}


bool SlidingCubesBlock::canMoveTo(const Cell3DPosition& dest) const {
    throw NotImplementedException("canMoveTo not implemented yet");
}

bool SlidingCubesBlock::moveTo(const Cell3DPosition& dest) {
    throw NotImplementedException("moveTo not implemented yet");
}

}
