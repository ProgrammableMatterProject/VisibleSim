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
#include "trace.h"

using namespace std;

namespace Catoms2D {

Catoms2DBlock::Catoms2DBlock(int bId, Catoms2DBlockCode *(*catoms2DBlockCodeBuildingFunction)(Catoms2DBlock*)) : BaseSimulator::BuildingBlock(bId) {
    OUTPUT << "Catoms2DBlock constructor" << endl;
    for (int i=0; i<MAX_NB_NEIGHBORS; i++) {
	tabInterfaces[i] = new P2PNetworkInterface(this);
	getP2PNetworkInterfaceList().push_back(tabInterfaces[i]);
    }
    buildNewBlockCode = catoms2DBlockCodeBuildingFunction;
    blockCode = (BaseSimulator::BlockCode*)buildNewBlockCode(this);
    angle = 0;
}

Catoms2DBlock::~Catoms2DBlock() {
    OUTPUT << "Catoms2DBlock destructor " << blockId << endl;
}

NeighborDirection::Direction Catoms2DBlock::getDirection(P2PNetworkInterface *p2p) {
    if (!p2p) {
	return NeighborDirection::Direction(0);
    }

    for (int i = 0; i < MAX_NB_NEIGHBORS; ++i) {
	if (getInterface(NeighborDirection::Direction(i)) == p2p) {
	    return NeighborDirection::Direction(i);
	}
    }
    return NeighborDirection::Direction(0);
}
  
Cell3DPosition Catoms2DBlock::getPosition(NeighborDirection::Direction d) {
    Cell3DPosition p = position;

    switch(d) {
    case NeighborDirection::BottomLeft:
	if ((abs(p[2])%2) == 0) {
	    p.pt[0]--;
	}
	p.pt[2]--;
	break;
    case NeighborDirection::Left:
	p.pt[0]--;
	break;
    case NeighborDirection::TopLeft:    
	if ((abs(p[2])%2) == 0) {
	    p.pt[0]--;
	}
	p.pt[2]++;
	break;
    case NeighborDirection::TopRight:
	if ((abs(p[2])%2) == 1) {
	    p.pt[0]++;
	}
	p.pt[2]++;
	break;
    case NeighborDirection::Right:
	p.pt[0]++;
	break;
    case NeighborDirection::BottomRight: 
	if ((abs(p[2])%2) == 1) {
	    p.pt[0]++;
	}
	p.pt[2]--;
	break;
    }
    return p;
}

Cell3DPosition Catoms2DBlock::getPosition(P2PNetworkInterface *p2p) {
    return getPosition(getDirection(p2p));
}

std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color << "\tpos: " << bb.position;
    return stream;
}

P2PNetworkInterface *Catoms2DBlock::getInterface(NeighborDirection::Direction d) {
    int alpha = angle;
    int beta = d*60;
    int t = beta-alpha;

    if (t>=0){
	return tabInterfaces[(t/60)%6];	
    } else {
	return tabInterfaces[((360+t)/60)%6];
    }
}
	
bool Catoms2DBlock::hasANeighbor(NeighborDirection::Direction n, bool groundIsNeighbor) {
    return hasANeighbor(getInterface(n),groundIsNeighbor);
}
	
bool Catoms2DBlock::hasANeighbor(P2PNetworkInterface *p2p, bool groundIsNeighbor) {
    Cell3DPosition p = getPosition(p2p);
    if(p2p->connectedInterface) {
	return true;
    } else if (groundIsNeighbor && (p[2]<0)) {
	return true;
    }
    return false;
}

int Catoms2DBlock::nbNeighbors(bool groundIsNeighbor) {
    int cnt = 0;
    for (int i = 0; i < MAX_NB_NEIGHBORS; i++) {
	if (hasANeighbor((NeighborDirection::Direction)i, groundIsNeighbor)) {
	    cnt++;
	}
    }
    return cnt;
}

int Catoms2DBlock::nbConsecutiveNeighbors(bool groundIsNeighbor) {
    int empty = -1;
    int m = 0;
    int cnt = 0;
    for(int i = 0; i < MAX_NB_NEIGHBORS; i++) { 
	if (!hasANeighbor((NeighborDirection::Direction)i, groundIsNeighbor)) {
	    empty = i;
	    break;
	}
    }
    
    if (empty == -1) {
	return MAX_NB_NEIGHBORS;
    }
	
    for( int i = 0; i < MAX_NB_NEIGHBORS; i++) {
	int j = (empty+i)%MAX_NB_NEIGHBORS;
	if (hasANeighbor((NeighborDirection::Direction)j, groundIsNeighbor)) {
	    cnt++;
	} else {
	    m = max(m,cnt);
	    cnt = 0;
	}
    }
    m = max(m,cnt);
    return m;
}

int Catoms2DBlock::nbConsecutiveEmptyFaces(bool groundIsNeighbor) {
    int notEmpty = -1;
    int m = 0;
    int cnt = 0;
    for(int i = 0; i < MAX_NB_NEIGHBORS; i++) { 
	if (hasANeighbor((NeighborDirection::Direction)i, groundIsNeighbor)) {
	    notEmpty = i;
	    break;
	}
    }
    
    if (notEmpty == -1) {
	return 0;
    }
	
    for( int i = 0; i < MAX_NB_NEIGHBORS; i++) {
	int j = (notEmpty+i)%MAX_NB_NEIGHBORS;
	if (!hasANeighbor((NeighborDirection::Direction)j, groundIsNeighbor)) {
	    cnt++;
	} else {
	    m = max(m,cnt);
	    cnt = 0;
	}
    }
    m = max(m,cnt);
    return m;
}


bool Catoms2DBlock::isBlocked() {
    int n = nbNeighbors(true);
    int nc = nbConsecutiveNeighbors(true);
    return (!((n == nc) && (nc <= 3))); 
}

P2PNetworkInterface* Catoms2DBlock::getNextInterface(RelativeDirection::Direction dir, P2PNetworkInterface *p2p, bool connected) {
    P2PNetworkInterface *next = NULL;
    int d = getDirection(p2p);

    do {
	if (dir == RelativeDirection::CW) {
	    if (d == NeighborDirection::Right) { 
		d= NeighborDirection::BottomRight; 
	    } else { 
		d--;
	    }
	} else {
	    d = (d+1)%MAX_NB_NEIGHBORS;
	}
	next = getInterface((NeighborDirection::Direction)d);
	if (!connected)  {
	    break;
	}
      
    } while((next->connectedInterface == NULL) && (next != p2p));
    return next;
}

	
	
int Catoms2DBlock::getCCWMovePivotId() {
    for (int j = 0; j < 6; j++) {
	P2PNetworkInterface *p2p = getInterface((NeighborDirection::Direction)j);
			
	if (p2p->connectedInterface) {
	    bool res = true;
	    for (int i = 1; i < 4; i++) {
		int dir = ((j + i) % 6);
		P2PNetworkInterface *p2pNeighbor = getInterface(dir);
		Cell3DPosition p = getPosition((NeighborDirection::Direction)dir);
		if (p2pNeighbor->connectedInterface || (p[2] < 0)) {
		    res = false;
		}
	    }
				
	    if (res)
		return p2p->getConnectedBlockId();
	}
    }
		
    return -1;
}

int Catoms2DBlock::getCWMovePivotId() {
    for (int j = 5; j > 0; j--) {
	P2PNetworkInterface *p2p = getInterface((NeighborDirection::Direction)j);
			
	if (p2p->connectedInterface) {
	    bool res = true;
	    for (int i = 1; i < 4; i++) {
		int dir = ((j - i)%6 + 6)%6;
		P2PNetworkInterface *p2pNeighbor = getInterface(dir);
		Cell3DPosition p = getPosition((NeighborDirection::Direction)dir);
		if (p2pNeighbor->connectedInterface || (p[2] < 0)) {
		    res = false;
		}
	    }
				
	    if (res)
		return p2p->getConnectedBlockId();
	}
    }
		
    return -1;
}

	
// Motion
bool Catoms2DBlock::canMove(Catoms2DMove &m) {
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
	    if (p2pDirection == NeighborDirection::BottomRight) {
		p2pDirection = NeighborDirection::Right;
	    } else {
		p2pDirection++;
	    }
	} else if (direction == RelativeDirection::CCW) {
	    if (p2pDirection == NeighborDirection::Right) {
		p2pDirection = NeighborDirection::BottomRight;
	    } else {
		p2pDirection--;
	    }
	}
	P2PNetworkInterface *p2p = getInterface((NeighborDirection::Direction)p2pDirection);
	Cell3DPosition p = getPosition((NeighborDirection::Direction)p2pDirection);
	if (p2p->connectedInterface || (p[2] < 0)) {
	    //cout << "somebody is connected there" << endl;
	    res = false;
	}
    }
  
    return res;
}

void Catoms2DBlock::startMove(Catoms2DMove &m, uint64_t t) {
    getScheduler()->schedule(new MotionStartEvent(t,this,m));
}

void Catoms2DBlock::startMove(Catoms2DMove &m) {
    startMove(m,getScheduler()->now());
}

// inline string Catoms2DBlock::xmlBuildingBlock() {       
//   return "\t\t<block position=" + ConfigUtils::Vector3D3DToXmlString(position)
//     + " color=" + ConfigUtils::colorToXmlString(color) + " />\n";
// }

}
