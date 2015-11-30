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

  string NeighborDirection::getString(int d) {
    switch(d) {
    case BottomLeft:
      return string("Bottom-left");
      break;
    case Left:
      return string("Left");
      break;
    case TopLeft:
      return string("Top-left");
      break;
    case BottomRight:
      return string("Bottom-right");
      break;
    case Right:
      return string("Right");
      break;
    case TopRight:
      return string("Top-right");
      break;
    default:
      cerr << "Unknown direction" << endl;
      return string("Unknown");
      break;
    }
  }

  int NeighborDirection::getOpposite(int d) {
    switch (Direction(d)) {
    case BottomLeft:
      return TopRight;
      break;
    case Left:
      return Right;
      break;
    case TopLeft:
      return BottomRight;
      break;
    case BottomRight:
      return TopLeft;
      break;
    case Right:
      return Left;
      break;
    case TopRight:
      return BottomLeft;
      break;
    default:
      ERRPUT << "*** ERROR *** : unknown face" << endl;
      return -1;
      break;
    }
  }

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

  void Catoms2DBlock::setPosition(const Vecteur &p) {
    position=p;
    getWorld()->updateGlData(this);
  }

  void Catoms2DBlock::setColor(const Color &c) {
    color = c;
    getWorld()->updateGlData(this,Vecteur(ptrGlBlock->position[0],ptrGlBlock->position[1],ptrGlBlock->position[2]),ptrGlBlock->angle);
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
  
  Vecteur Catoms2DBlock::getPosition(NeighborDirection::Direction d) {
    Vecteur p = position;

    switch(d) {
    case NeighborDirection::BottomLeft:
      if ((abs((int)p[2])%2) == 0) {
	p.pt[0]--;
      }
      p.pt[2]--;
      break;
    case NeighborDirection::Left:
      p.pt[0]--;
      break;
    case NeighborDirection::TopLeft:    
      if ((abs((int)p[2])%2) == 0) {
	p.pt[0]--;
      }
      p.pt[2]++;
      break;
    case NeighborDirection::TopRight:
      if ((abs((int)p[2])%2) == 1) {
	p.pt[0]++;
      }
      p.pt[2]++;
      break;
    case NeighborDirection::Right:
      p.pt[0]++;
      break;
    case NeighborDirection::BottomRight: 
      if ((abs((int)p[2])%2) == 1) {
	p.pt[0]++;
      }
      p.pt[2]--;
      break;
    }
    return p;
  }
  Vecteur Catoms2DBlock::getPosition(P2PNetworkInterface *p2p) {
    return getPosition(getDirection(p2p));
  }

  std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
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
    Vecteur p = getPosition(p2p);
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
  
  // Motion
  bool Catoms2DBlock::canMove(Catoms2DMove &m) {  
    // physical moving condition
    // pivot is a neighbor (physically connected)
    // move CW around i connector: i+1, i+2 and i+3 should be free
    // move CCW around i connector: i-1, i-2 and i-3 should be free

    Catoms2DMove::direction_t direction = m.getDirection();
    Catoms2DBlock *pivot = m.getPivot();

    if ((direction != Catoms2DMove::ROTATE_CW) && 
	(direction != Catoms2DMove::ROTATE_CCW)) {
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
      if (direction == Catoms2DMove::ROTATE_CW) {
	if (p2pDirection == NeighborDirection::BottomRight) {
	  p2pDirection = NeighborDirection::Right;
	} else {
	  p2pDirection++;
	}
      } else if (direction == Catoms2DMove::ROTATE_CCW) {
	if (p2pDirection == NeighborDirection::Right) {
	  p2pDirection = NeighborDirection::BottomRight;
	} else {
	  p2pDirection--;
	}
      }
      P2PNetworkInterface *p2p = getInterface((NeighborDirection::Direction)p2pDirection);
      Vecteur p = getPosition((NeighborDirection::Direction)p2pDirection);
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

}
