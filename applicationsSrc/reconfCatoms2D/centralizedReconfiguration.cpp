#include <iostream>
#include "centralizedReconfiguration.h"
#include "bfs.h"
#include "catoms2DWorld.h"
#include "coordinate.h"
#include "catoms2DMove.h"
#include "map.h"

using namespace std;
using namespace Catoms2D;

#define COLOR_DEBUG
#define ROTATION_DIRECTION Catoms2DMove::ROTATE_CW
#define UNDEFINED_GRADIENT -1

#define CONSECUTIVE_NEIGHBORS
#define NB_MAX_CONSECUTIVE_NEIGHBORS_TO_MOVE 3
//#define GRADIENT

enum state_t {MOVING = 0, UNMOVING = 1, STABLE = 2, UNKNOWN = 3};

static int moves = 0;

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

static bool isNeighbor(Catoms2DBlock *c,Coordinate p) {
  
  // test all neighbors cell
  for (int i = 0; i < 6; i++) {
    Vecteur v = c->getPosition((NeighborDirection::Direction)i);
    Coordinate ci(v[0],v[2]);
    if (ci == p) {
      return true;
    }
  }
  return false;
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
  if (d == Catoms2DMove::ROTATE_CCW) {
    if (p2pDirection == NeighborDirection::BottomRight) {
      p2pDirection = NeighborDirection::Right;
    } else {
      p2pDirection++;
    }
  } else if (d == Catoms2DMove::ROTATE_CW) {
    if (p2pDirection == NeighborDirection::Right) {
      p2pDirection = NeighborDirection::BottomRight;
    } else {
      p2pDirection--;
    }
  }
  return c->getInterface((NeighborDirection::Direction)p2pDirection);
}

static P2PNetworkInterface *extremeNeighborInDirection(Catoms2DBlock *c, Catoms2DMove::direction_t d) {
  P2PNetworkInterface *p1 = NULL, *p2 = NULL;
  
  if (c->nbNeighbors() == 0) {
    return NULL;
  }

  int cn = c->nbConsecutiveNeighbors();
  
  // pick-up a neighbor of c in the longest sequence of consecutive neighbors
  for (int i = 0; i < 6; i++) {
    int n = 0;
    
    p1 = c->getInterface((NeighborDirection::Direction)i);
    if (!p1->connectedInterface) {
      continue;
    }
    n = 1;
    p2 = p1;
    
    while (n != cn) {
      p2 = nextInterface(c,d,p2);
      if(!p2->connectedInterface) {
	break;
      }
      n++;
    }
    if (n == cn) {
      break;
    }
  }

  if (c->nbNeighbors() == 1) {
    return p1;
  }
  
  p2 = p1;
  while (true) {
    p2 = nextInterface(c, d, p2);
    if (!p2->connectedInterface) {
      return p1;
    }
    p1 = p2;
  }
}

static  Catoms2DMove::direction_t reverseDirection(Catoms2DMove::direction_t d) {
  if (d == Catoms2DMove::ROTATE_CCW) {
    return Catoms2DMove::ROTATE_CW;
  } else if (d == Catoms2DMove::ROTATE_CW) {
    return Catoms2DMove::ROTATE_CCW;
  }
  return Catoms2DMove::ROTATE_CW; // default
}

static P2PNetworkInterface *lastNeighborInDirection(Catoms2DBlock *c, Catoms2DMove::direction_t d) {
  // the last one is the first one in the opposite direction
  return extremeNeighborInDirection(c,reverseDirection(d));
}

static P2PNetworkInterface *firstNeighborInDirection(Catoms2DBlock *c, Catoms2DMove::direction_t d) {
  return extremeNeighborInDirection(c,d);
}

static P2PNetworkInterface* previousInterfacePerimeter(Catoms2DBlock  *c) {
   return firstNeighborInDirection(c,ROTATION_DIRECTION);
}

static P2PNetworkInterface* nextInterfacePerimeter(Catoms2DBlock  *c) {
   return lastNeighborInDirection(c,ROTATION_DIRECTION);
}

static Catoms2DBlock* nextCatomPerimeter(Catoms2DBlock  *c) {
  return (Catoms2DBlock*)nextInterfacePerimeter(c)->connectedInterface->hostBlock;
}

static Catoms2DBlock* previousCatomPerimeter(Catoms2DBlock  *c) {
  return (Catoms2DBlock*)previousInterfacePerimeter(c)->connectedInterface->hostBlock;
}

static Catoms2DMove* nextMove(Catoms2DBlock  *c) {
  Catoms2DBlock* pivot = nextCatomPerimeter(c);
  Catoms2DMove m(pivot,ROTATION_DIRECTION);
  if (c->canMove(m)) {
    //cout << c->blockId << " can move arround " << pivot->blockId << endl;
    return new Catoms2DMove(m);
  }
  return NULL;
}

static Coordinate getPosition(Catoms2DBlock* c, Catoms2DMove &m) {
  P2PNetworkInterface *p2p = m.getPivot()->getP2PNetworkInterfaceByBlockRef(c);
  Coordinate position(m.getPivot()->position[0], m.getPivot()->position[2]);
  p2p = nextInterface(m.getPivot(),m.getDirection(),p2p);
  return Map::getPosition(m.getPivot(),position,p2p);
}

static bool isInTarget(Coordinate &p) { 
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  return (world->getTargetGrid(p.x,0,p.y) == fullCell);
}

static bool canMove(Catoms2DBlock *c, Catoms2DMove &m, state_t states[]) {
  Coordinate g = getPosition(c,m);
  Catoms2DBlock *next = m.getPivot();
  int i = 0;
  while (isNeighbor(next,g)) {
    i++;
    
    if (states[next->blockId] == UNKNOWN) {
      cerr << "initialization error!" << endl;
    }

    if (states[next->blockId] == MOVING) {
      cout << c->blockId << " can't move because of " << next->blockId << endl;
      return false;
    }

    if (i == 4) {
      cout << c->blockId << " can't because i == 4 " << next->blockId << endl;
      return false;
    }

    next = nextCatomPerimeter(next);
  }
  return true; 
}

static bool canMove(Catoms2DBlock *c, int gradient[]) {
#ifdef CONSECUTIVE_NEIGHBORS
  // on the border (r)
  int nbNeighbors = c->nbNeighbors();
  int nbConsecutiveNeighbors = c->nbConsecutiveNeighbors();
  bool r = (nbNeighbors <= NB_MAX_CONSECUTIVE_NEIGHBORS_TO_MOVE) &&
    (nbConsecutiveNeighbors == nbNeighbors);
  return r;
#elif defined(GRADIENT)
  // can algorithmically move (gradient higher or equal than all neighbors)
  int id1 = c->blockId; 

  if (gradient[id1] ==  UNDEFINED_GRADIENT) {
    return false;
  }

  for (int i = 0; i < 6; i++) {
    P2PNetworkInterface *p2p = c->getInterface((NeighborDirection::Direction)i);
    if (p2p->connectedInterface) {
      int id2 = p2p->connectedInterface->hostBlock->blockId;  

      if (gradient[id2] ==  UNDEFINED_GRADIENT) {
	return false;
      }

      if (gradient[id1] < gradient[id2]) {
	return false;
      }
    }
  }
  return true;
#endif
}

static void move(Catoms2DBlock* c, Catoms2DMove &m) {
  if (!c->canMove(m)) {
    cerr << "error illegal move" << endl;
    return;
  } 
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  // final position
  Coordinate p = getPosition(c,m);
  // disconnect
  world->disconnectBlock(c);
  // rotate
  c->angle += 60*m.getDirection();
  // set grid and update gl world
  world->setGridPtr(p.x,0,p.y,c);
  c->setPosition(Vecteur(p.x,0,p.y)); 
  world->updateGlData(c,world->gridToWorldPosition(c->position),c->angle);
  // connect
  world->connectBlock(c);
  moves++;
}

static void updateGradient(Catoms2DBlock *c, int gradient[]) {
// can algorithmically move (gradient higher or equal than all neighbors)
  int id1 = c->blockId;
  int minGradient = INT_MAX;
    
  for (int i = 0; i < 6; i++) {
    P2PNetworkInterface *p2p = c->getInterface((NeighborDirection::Direction)i);
    if (p2p->connectedInterface) {
      int id2 = p2p->connectedInterface->hostBlock->blockId;
      if (gradient[id2] == UNDEFINED_GRADIENT) {
	cerr << "error: update gradient" << endl;
      }
      minGradient = min(minGradient,gradient[id2]);
    }
  }
  gradient[id1] = minGradient + 1;
}

void centralized_reconfiguration() {
  cout << "centralized reconfiguration" << endl;
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  Catoms2DBlock *seed = NULL;
  int gradient[world->getSize()+1];
  state_t states[world->getSize()+1];
  Tree *bfs = NULL;

  Coordinate mapBottomRight = getMapBottomRight();
  seed = world->getGridPtr(mapBottomRight.x,0,mapBottomRight.y);
#ifdef COLOR_DEBUG
  seed->setColor(RED);
#endif

  for (int i = 0; i < (world->getSize()+1); i++) {
    gradient[i] = UNDEFINED_GRADIENT;
    states[i] = UNKNOWN;
    //Catoms2DBlock *c = (Catoms2DBlock*) world->getBlockById(i);
  }
  states[seed->blockId] = STABLE;
  
  bfs = Tree::bfs(seed->blockId,gradient,world->getSize());
  
  //enum state_t {MOVING = 0, UNMOVING = 1, STABLE = 2, UNKNOWN = 3};
  
  while (!isOver()) {
    // algorithm moving condition of catom c1:
    // FALSE ???:
    // c1 gradient will be lower in the destination cell.
    // none of the gradient of c1's neighbors will change (ie gradient is lower or equal to c1).
    
    // physical moving condition
    // move CW around i connector: i+1 and i+2 should be free
    // move CCW around i connector: i-1 and i-2 should be free
    Catoms2DWorld *world = Catoms2DWorld::getWorld();
    Catoms2DBlock *c;

    map<int, BuildingBlock*>::iterator it;
    for (it=world->getMap().begin() ; it != world->getMap().end(); ++it) {

      c = (Catoms2DBlock*) it->second;

      if (states[c->blockId] == STABLE) {
	c->setColor(GREEN);
	continue;
      }
      
      if (c->isBlocked()) {
	c->setColor(GREY);
	if (states[c->blockId] == MOVING) {
	  cerr << "error: mv -> unmv" << endl;
	}
	states[c->blockId] = UNMOVING;
	continue;
      } else {
	if (canMove(c,gradient)) {
	  Catoms2DMove *mv = nextMove(c);
	  if (mv != NULL) {
	    Coordinate p1(c->position[0], c->position[2]);
	    Coordinate p2 = getPosition(c,*mv);
	    if ((!isInTarget(p1) || (isInTarget(p1) && isInTarget(p2) && (p2.y <= p1.y)))) {
	      states[c->blockId] = MOVING;
	      c->setColor(RED);
	    } else {
	      states[c->blockId] = STABLE;
	      c->setColor(GREEN);
	    }
	  }
	}
      }
    }

    getchar();

    for (it=world->getMap().begin() ; it != world->getMap().end(); ++it) {

      c = (Catoms2DBlock*) it->second;
      
      if (states[c->blockId] == STABLE) {
	continue;
      }
      
      if (c->isBlocked()) {
	continue;
      }

      if (canMove(c,gradient)) {
	//cout << "c satisfies gradient condition" << endl;
	//cout << "@" << c->blockId << " can physically move" << endl;
	Catoms2DMove *mv = nextMove(c);
	if (mv != NULL) {
	  Coordinate p1(c->position[0], c->position[2]);
	  Coordinate p2 = getPosition(c,*mv);
      
	  if (!canMove(c,*mv,states)) {
	    continue;
	  }
	  
	  if ((!isInTarget(p1) || (isInTarget(p1) && isInTarget(p2) && (p2.y <= p1.y)))) {
	    cout << c->blockId << " is moving from " << p1 << " to " 
		 << p2 << " using " << mv->getPivot()->blockId << " in direction " << mv->getDirection(); 
	    move(c,*mv);
	    //cout << c->blockId << " has " << c->nbNeighbors() << " neighbors" << endl; 
	    gradient[c->blockId] = UNDEFINED_GRADIENT;
	    updateGradient(c,gradient);
	    cout << " done" << endl;
	    
	    //Catoms2DMove counterMV(mv->getPivot(),reverseDirection(mv->getDirection()));
	    //if (!c->canMove(counterMV)) {
	    if (c->isBlocked()) {
	      //c->setColor(BLUE);
	      //mv->getPivot()->setColor(YELLOW);
	      cout << "illegal move!" << endl;
	      //getchar();
	    }
	    getchar();
	    //sleep(1);
	  } /*else {
	    cout << "hors figure ?" << endl;
	    }*/
	  delete mv;
	}// else { cout << " move == NULL" << endl;}
      }
#ifdef COLOR_DEBUG
      Coordinate p1 = Coordinate(c->position[0], c->position[2]);
      if (isInTarget(p1)) {
	c->setColor(GREEN);
      } else {
	c->setColor(GREY);
      }
#endif
    }
  }
  cout << "reconfiguration over in " << moves << " moves." << endl;
}
