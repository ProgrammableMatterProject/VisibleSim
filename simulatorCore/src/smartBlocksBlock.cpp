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
static const GLfloat tabColors[12][4]={{1.0,0.0,0.0,1.0},{1.0,0.647058824,0.0,1.0},{1.0,1.0,0.0,1.0},
                                       {0.0,1.0,0.0,1.0}, {0.0,0.0,1.0,1.0},
                                       {0.274509804,0.509803922,0.705882353,1.0},
                                       {0.815686275,0.125490196,0.564705882,1.0},{0.5,0.5,0.5,1.0},
                                       {0.980392157,0.5,0.456,1.0},{0.549019608,0.85,0.5,1.0},
                                       {0.980392157,0.843137255,0.0,1.0},
                                       {0.094117647,0.545098039,0.094117647,1.0}};

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
    
SmartBlocksBlock::SmartBlocksBlock(int bId,
                                   SmartBlocksBlockCode *
                                   (*smartBlocksBlockCodeBuildingFunction)(SmartBlocksBlock*))
    : BaseSimulator::BuildingBlock(bId) {
    OUTPUT << "SmartBlocksBlock #" << bId << " constructor" << endl;
    buildNewBlockCode = smartBlocksBlockCodeBuildingFunction;
    blockCode = (BaseSimulator::BlockCode*)buildNewBlockCode(this);
    for (int i=NeighborDirection::North; i<=NeighborDirection::West; i++) {
        tabInterfaces[i] = new P2PNetworkInterface(this);
    }
}

SmartBlocksBlock::~SmartBlocksBlock() {
    for (int i = NeighborDirection::North; i <= NeighborDirection::West; i++) {
        delete tabInterfaces[i];
    }
    OUTPUT << "SmartBlocksBlock #" << blockId << " destructor" << endl;
}

void SmartBlocksBlock::setPosition(const Vector3D &p) {
    position=p;
    getWorld()->updateGlData(this);
}

void SmartBlocksBlock::setColor(const Color &c) {
    color=c;
    getWorld()->updateGlData(this);
}

void SmartBlocksBlock::setColor(int num) {
    const GLfloat *col = tabColors[num%12];
    color.set(col[0],col[1],col[2],col[3]);
    getWorld()->updateGlData(this);
}

P2PNetworkInterface *SmartBlocksBlock::getP2PNetworkInterfaceByRelPos(const PointCel &pos) {
    if (pos.x==-1) return tabInterfaces[NeighborDirection::West];
    else if (pos.x==1) return tabInterfaces[NeighborDirection::East];
    else if (pos.y==-1) return tabInterfaces[NeighborDirection::South];
    else if (pos.y==1) return tabInterfaces[NeighborDirection::North];

    return NULL;
}

NeighborDirection::Direction SmartBlocksBlock::getDirection(P2PNetworkInterface *given_interface) {
    /*if( !given_interface) {
      return NeighborDirection::Direction(0);
      }*/
    for( int i( NeighborDirection::North); i <= NeighborDirection::West; ++i) {
        P2PNetworkInterface* p2p = tabInterfaces[ i];
        if( p2p == given_interface) {
            return NeighborDirection::Direction(i);
        }
    }
    assert(0);			// should never get here
}

Vector3D SmartBlocksBlock::getPosition(NeighborDirection::Direction d) {
    Vector3D p = position;

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
    while (i<4 && (tabInterfaces[i]->connectedInterface == NULL
                   || tabInterfaces[i]->connectedInterface->hostBlock->blockId != id)) {
        i++;
    }
    return (i<4?tabInterfaces[i]:NULL);
}
    
}
