/*
 * SmartBlocksBlock.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include "smartBlocksBlock.h"
#include "smartBlocksWorld.h"

using namespace std;

namespace SmartBlocks {

int NeighborDirection::getOpposite(int d) {
    switch(d) {
    case NeighborDirection::North :
        return South;
        break;
    case NeighborDirection::East :
        return West;
        break;
    case NeighborDirection::South :
        return North;
        break;
    case NeighborDirection::West :
        return East;
        break;
    default:
        ERRPUT << "*** ERROR *** : unknown face" << endl;
        return -1;
        break;
    }

}
    
string NeighborDirection::getString(int d) {
    switch(d) {
    case NeighborDirection::North :
        return string("North");
        break;
    case NeighborDirection::East :
        return string("East");
        break;
    case NeighborDirection::South :
        return string("South");
        break;
    case NeighborDirection::West :
        return string("West");
        break;
    default:
        cerr << "Unknown direction" << endl;
        return string("Unknown");
        break;
    }
}
    
SmartBlocksBlock::SmartBlocksBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb) {
    OUTPUT << "SmartBlocksBlock #" << bId << " constructor" << endl;

    for (int i=NeighborDirection::North; i<=NeighborDirection::West; i++) {
        P2PNetworkInterfaces.push_back(new P2PNetworkInterface(this));
    }
}

SmartBlocksBlock::~SmartBlocksBlock() {
    OUTPUT << "SmartBlocksBlock #" << blockId << " destructor" << endl;
}

P2PNetworkInterface *SmartBlocksBlock::getP2PNetworkInterfaceByRelPos(const PointCel &pos) {
    if (pos.x==-1) return P2PNetworkInterfaces[NeighborDirection::West];
    else if (pos.x==1) return P2PNetworkInterfaces[NeighborDirection::East];
    else if (pos.y==-1) return P2PNetworkInterfaces[NeighborDirection::South];
    else if (pos.y==1) return P2PNetworkInterfaces[NeighborDirection::North];

    return NULL;
}

NeighborDirection::Direction SmartBlocksBlock::getDirection(P2PNetworkInterface *given_interface) {
    /*if( !given_interface) {
      return NeighborDirection::Direction(0);
      }*/
    for( int i( NeighborDirection::North); i <= NeighborDirection::West; ++i) {
        P2PNetworkInterface* p2p = P2PNetworkInterfaces[ i];
        if( p2p == given_interface) {
            return NeighborDirection::Direction(i);
        }
    }
    assert(0);			// should never get here
}

Cell3DPosition SmartBlocksBlock::getPosition(NeighborDirection::Direction d) {
    Cell3DPosition p = position;

    switch (d) {
    case NeighborDirection::North :
        p.pt[1]++;
        break;
    case NeighborDirection::East :
        p.pt[0]--;
        break;
    case NeighborDirection::South :
        p.pt[1]--;
        break;
    case NeighborDirection::West :
        p.pt[0]++;
        break;        
    }

    
    return p;
}

P2PNetworkInterface *SmartBlocksBlock::getP2PNetworkInterfaceByDestBlockId(int id) {
    int i=0;
    while (i<4 && (P2PNetworkInterfaces[i]->connectedInterface == NULL
                   || P2PNetworkInterfaces[i]->connectedInterface->hostBlock->blockId != id)) {
        i++;
    }
    return (i<4?P2PNetworkInterfaces[i]:NULL);
}
    
}
