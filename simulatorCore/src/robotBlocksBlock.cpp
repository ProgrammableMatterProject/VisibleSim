/*
 * robotBlocksBlock.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>

#include "robotBlocksBlock.h"
#include "robotBlocksWorld.h"
#include "robotBlocksSimulator.h"
#include "lattice.h"
#include "trace.h"

using namespace std;

namespace RobotBlocks {

RobotBlocksBlock::RobotBlocksBlock(int bId, BlockCodeBuilder bcb)
	: BaseSimulator::BuildingBlock(bId, bcb, SCLattice::MAX_NB_NEIGHBORS) {
    OUTPUT << "RobotBlocksBlock constructor" << endl;
}

RobotBlocksBlock::~RobotBlocksBlock() {
    OUTPUT << "RobotBlocksBlock destructor " << blockId << endl;
}

void RobotBlocksBlock::setPrevNext(int prev,int next) {
    getWorld()->updateGlData(this,prev,next);
}

void RobotBlocksBlock::setPrevNext(const P2PNetworkInterface *prev,const P2PNetworkInterface *next) {
    int prevId=0,nextId=0;
    if (prev) {
		RobotBlocksBlock*rb = (RobotBlocksBlock*)(prev->hostBlock);
		prevId = rb->blockId;
    }
    if (next) {
		RobotBlocksBlock*rb = (RobotBlocksBlock*)(next->hostBlock);
		nextId = rb->blockId;
    }
    //cout << (prev?prev->hostBlock->blockId:-1) << "," << (next?next->hostBlock->blockId:-1) << endl;
    getWorld()->updateGlData(this,prevId,nextId);
}

void RobotBlocksBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
		   << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
    getScheduler()->schedule(
		new AddNeighborEvent(getScheduler()->now(), this,
							 getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
}

void RobotBlocksBlock::removeNeighbor(P2PNetworkInterface *ni) {
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
		   << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
    getScheduler()->schedule(
		new RemoveNeighborEvent(getScheduler()->now(), this,
								getWorld()->lattice->getOppositeDirection(getDirection(ni))));
}

int RobotBlocksBlock::getDirection(P2PNetworkInterface *given_interface) {
    if( !given_interface) {
		return SCLattice::Direction(0);
    }
    for( int i(0); i < 6; ++i) {
		if(P2PNetworkInterfaces[i] == given_interface) return SCLattice::Direction(i);
    }
    return SCLattice::Direction(0);
}

P2PNetworkInterface *RobotBlocksBlock::getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos) {
    if (pos[0]==-1) return P2PNetworkInterfaces[SCLattice::Left];
    else if (pos[0]==1) return P2PNetworkInterfaces[SCLattice::Right];
    else if (pos[1]==-1) return P2PNetworkInterfaces[SCLattice::Front];
    else if (pos[1]==1) return P2PNetworkInterfaces[SCLattice::Back];
    else if (pos[2]==-1) return P2PNetworkInterfaces[SCLattice::Bottom];
    else if (pos[2]==1) return P2PNetworkInterfaces[SCLattice::Top];

    return NULL;
}

std::ostream& operator<<(std::ostream &stream, RobotBlocksBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
    return stream;
}

}
