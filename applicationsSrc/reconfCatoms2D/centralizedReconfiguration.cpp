#include <iostream>
#include "centralizedReconfiguration.h"
#include "bfs.h"
#include "catoms2DWorld.h"
#include "coordinate.h"
#include "catoms2DMove.h"

using namespace std;
using namespace Catoms2D;

#define COLOR_DEBUG
#define ROTATION_DIRECTION Catoms2DMove::ROTATE_CW
#define OTHER_DIRECTION Catoms2DMove::ROTATE_CCW

static Coordinate getMapBottomRight() {
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  Catoms2DBlock  *c;
  Coordinate rb(INT_MIN,INT_MAX);

  map<int, BuildingBlock*>::iterator it;
  for (it=world->getMap().begin() ; it != world->getMap().end(); ++it) {
    c = (Catoms2DBlock*) it->second;
    Coordinate cc(c->position[0], c->position[2]);
    /*if (cc.x >= rb.x) {
      rb.x = cc.x;
      if (cc.y <= rb.y) {
	rb.y = cc.y;
      }
      }*/
    if (cc.y <= rb.y) {
      rb.y = cc.y;
      if (cc.x >= rb.x) {
	rb.x = cc.x;
      }
    }
  }
  return rb;
}

static Coordinate getTargetBottomLeft() {
  Coordinate p(INT_MIN,INT_MIN);
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  int *gridSize = world->getGridSize();
  for (int iy = 0; iy < gridSize[2]; iy++) {
    for (int ix = 0; ix < gridSize[0]; ix++) {
      if (world->getTargetGrid(ix,0,iy) == fullCell ) {
	Coordinate cc(ix, iy);
	if (cc.y <= p.y) {
	  p.y = cc.y;
	  if (cc.x <= p.x) {
	    p.x = cc.x;
	  }
	}
      }
    }
  }
  return p;
}

static bool isOver() {
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  int *gridSize = world->getGridSize();
  for (int iy = 0; iy < gridSize[2]; iy++) {
    for (int ix = 0; ix < gridSize[0]; ix++) {
      if (world->getTargetGrid(ix,0,iy) == fullCell ) {
        if (!world->getGridPtr(ix,0,iy)) {
	  return false;
	}
      }
    }
  }
  return true;
}

static P2PNetworkInterface* 
nextInterface(Catoms2DBlock *c, Catoms2DMove::direction_t d, P2PNetworkInterface *p2p) {
  int p2pDirection = c->getDirection(p2p);

  if (d == Catoms2DMove::ROTATE_CW) {
    if (p2pDirection == NeighborDirection::BottomRight) {
      p2pDirection = NeighborDirection::Right;
    } else {
	 p2pDirection++;
    }
  } else if (d == Catoms2DMove::ROTATE_CCW) {
    if (p2pDirection == NeighborDirection::Right) {
      p2pDirection = NeighborDirection::BottomRight;
    } else {
      p2pDirection--;
    }
  }
  return c->getInterface((NeighborDirection::Direction)p2pDirection);
}

static Catoms2DMove* nextMove(Catoms2DBlock  *c) {
  P2PNetworkInterface *p1 = NULL, *p2 = NULL;
  Catoms2DBlock  *pivot;
  int i = 0;
  
  // pick-up a neighbor of c
  for (i = 0; i < 6; i++) {
    p1 = c->getInterface((NeighborDirection::Direction)i);
    if (!p1->connectedInterface) {
      break;
    }
  }
  
  if ((i == 5) && !p1) {
    return NULL;
  }

  p2 = p1;
  while (true) {
    if (ROTATION_DIRECTION == Catoms2DMove::ROTATE_CCW) {
      p2 = nextInterface(c, Catoms2DMove::ROTATE_CW, p2);
    } else if (ROTATION_DIRECTION == Catoms2DMove::ROTATE_CW) {
      p2 = nextInterface(c, Catoms2DMove::ROTATE_CCW, p2);
    }
    if (p2->connectedInterface) {
      pivot = (Catoms2DBlock*)p2->connectedInterface->hostBlock;
      Catoms2DMove m(pivot,ROTATION_DIRECTION);
      if (c->canMove(m)) {
	return new Catoms2DMove(m);
      }
    }
    if (p1 == p2) {
      return NULL;
    }
  }
}

void centralized_reconfiguration() {
  cout << "centralized reconfiguration" << endl;
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  Catoms2DBlock *seed = NULL;
  int gradient[world->getSize()];
  Tree *bfs = NULL;
  
  Coordinate mapBottomRight = getMapBottomRight();
  seed = world->getGridPtr(mapBottomRight.x,0,mapBottomRight.y);
#ifdef COLOR_DEBUG
  seed->setColor(RED);
#endif
  bfs = Tree::bfs(seed->blockId,gradient,world->getSize());

  while (!isOver()) {
    // algorithm moving condition of catom c1:
    // FALSE:
    // c1 gradient will be lower in the destination cell.
    // none of the gradient of c1's neighbors will change (ie gradient is lower or equal to c1).
    
    // physical moving condition
    // move CW around i connector: i+1 and i+2 should be free
    // move CCW around i connector: i-1 and i-2 should be free
    
  }
  /*Coordinate targetBottomLeft = getTargetBottomLeft();
  Coordinate 
  Coordinate trTargetBottomLeft = Map::real2Virtual(*/


  /* Current map:
     int *gridSize = world->getGridSize();
     for (int iy = 0; iy < gridSize[2]; iy++) {
     for (int ix = 0; ix < gridSize[0]; ix++) {
     Catoms2DBlock* c = world->getGridPtr(ix,0,iy);
     if (c != NULL) {
	
     }
     }
     }*/

  /* Target map:
  Coordinate pmin(INT_MAX,INT_MAX);
  Coordinate pmax(INT_MIN,INT_MIN);
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  int *gridSize = world->getGridSize();
  for (int iy = 0; iy < gridSize[2]; iy++) {
    for (int ix = 0; ix < gridSize[0]; ix++) {
      if (world->getTargetGrid(ix,0,iy) == fullCell ) {
	pmin.x = min(pmin.x, ix);
	pmin.y = min(pmin.y, iy);
	pmax.x = max(pmax.x, ix);
	pmax.y = max(pmax.y, iy);
	Coordinate real(ix,iy);
	Coordinate t =  map.real2Virtual(real);
	cout << "target: (" << t.x << " " << t.y << ")" << endl;
	ctuples.out(ContextTuple(string("target"),t));
      }
    }
  }
  Rectangle bounds(pmin,pmax);
  CTuple::setBounds(bounds);
  */
}
