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
    
SmartBlocksBlock::SmartBlocksBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb) {
    OUTPUT << "SmartBlocksBlock #" << bId << " constructor" << endl;

    for (int i=SLattice::North; i<=SLattice::West; i++) {
        P2PNetworkInterfaces.push_back(new P2PNetworkInterface(this));
    }
}

SmartBlocksBlock::~SmartBlocksBlock() {
    OUTPUT << "SmartBlocksBlock #" << blockId << " destructor" << endl;
}

P2PNetworkInterface *SmartBlocksBlock::getP2PNetworkInterfaceByRelPos(const PointCel &pos) {
    if (pos.x==-1) return P2PNetworkInterfaces[SLattice::West];
    else if (pos.x==1) return P2PNetworkInterfaces[SLattice::East];
    else if (pos.y==-1) return P2PNetworkInterfaces[SLattice::South];
    else if (pos.y==1) return P2PNetworkInterfaces[SLattice::North];

    return NULL;
}

SLattice::Direction SmartBlocksBlock::getDirection(P2PNetworkInterface *given_interface) {
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

Cell3DPosition SmartBlocksBlock::getPosition(SLattice::Direction d) {
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

P2PNetworkInterface *SmartBlocksBlock::getP2PNetworkInterfaceByDestBlockId(int id) {
    int i=0;
    while (i<4 && (P2PNetworkInterfaces[i]->connectedInterface == NULL
                   || P2PNetworkInterfaces[i]->connectedInterface->hostBlock->blockId != id)) {
        i++;
    }
    return (i<4?P2PNetworkInterfaces[i]:NULL);
}
    
}
