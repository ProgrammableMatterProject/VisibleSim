/**
 * @file   nodeBlock.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:47:36 2019
 *
 * @brief
 *
 *
 */

#include <iostream>

#include "nodeBlock.h"
#include "buildingBlock.h"
#include "nodeWorld.h"
#include "nodeSimulator.h"
#include "trace.h"

using namespace std;

//! \namespace Node
namespace Node {

NodeBlock::NodeBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb, SLattice::MAX_NB_NEIGHBORS) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "NodeBlock constructor" << endl;
#endif
}

NodeBlock::~NodeBlock() {
    OUTPUT << "NodeBlock destructor " << blockId << endl;
}

int NodeBlock::getDirection(P2PNetworkInterface *given_interface) const {
    if( !given_interface) {
        return -1;
    }

    for( int i(0); i < SLattice::MAX_NB_NEIGHBORS; ++i) {
        if(P2PNetworkInterfaces[i] == given_interface) return i;
    }

    return -1;
}

std::ostream& operator<<(std::ostream &stream, NodeBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
    return stream;
}

bool NodeBlock::getNeighborPos(SLattice::Direction connectorDir,Cell3DPosition &pos) const {
    NodeWorld *wrl = getWorld();
    pos = position+((SLattice*)(wrl->lattice))->getNeighborRelativePos(connectorDir);
    return wrl->lattice->isInGrid(pos);
}

void NodeBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
        new AddNeighborEvent(getScheduler()->now(), this,
                             getWorld()->lattice->getOppositeDirection(getDirection(ni)),
                             target->blockId));
}

void NodeBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(new RemoveNeighborEvent(getScheduler()->now(), this,getWorld()->lattice->getOppositeDirection(getDirection(ni))));
    getWorld()->updateGlData(this,getDirection(ni),0);
}

bool NodeBlock::hasANeighbor(SLattice::Direction n, bool groundIsNeighbor) const {
    return hasANeighbor(getInterface(n),groundIsNeighbor);
}

bool NodeBlock::hasANeighbor(P2PNetworkInterface *p2p, bool groundIsNeighbor) const {
    Cell3DPosition p = getPosition(p2p);
    if(p2p->connectedInterface) {
        return true;
    } else if (groundIsNeighbor && (p[2]<0)) {
        return true;
    }
    return false;
}

Cell3DPosition NodeBlock::getPosition(SLattice::Direction d) const {
    World *wrl = getWorld();
    const vector<Cell3DPosition>& nCells = wrl->lattice->getRelativeConnectivity(position);
    return position + nCells[d];
}

// PTHY: TODO: Can be genericized in BuildingBlocks
Cell3DPosition NodeBlock::getPosition(P2PNetworkInterface *p2p) const{
    return getPosition((SLattice::Direction)getDirection(p2p));
}

}
