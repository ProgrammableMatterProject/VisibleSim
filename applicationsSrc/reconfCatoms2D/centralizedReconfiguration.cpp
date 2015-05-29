#include <iostream>
#include "centralizedReconfiguration.h"
#include "bfs.h"
#include "catoms2DWorld.h"
#include "coordinate.h"

using namespace std;
using namespace Catoms2D;

#define COLOR_DEBUG

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

#define CLOCKWISE 1
#define COUNTERCLOCKWISE 2
static void move(Catoms2DBlock  *c, int sens) {

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
