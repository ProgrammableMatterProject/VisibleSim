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

/**
 * Strategy:
 * 1: let clockwise pivot moves first
 * 2: let counter-clockwise pivot moves first
 * 3: highest catom moves first
 * 4: node OUT_SHAPE on the perimeter closer from the node IN_SHAPE that has
 *    the lowest neighbor cell to fill.
 **/


//#Define STRATEGY_ONE
#define STRATEGY_TWO
//#define STRATEGY_THREE
//#define STRATEGY_FOUR

#define STRATEGY_TRHEE
enum state_t {IN_SHAPE = 0, OUT_SHAPE = 1, WELL_PLACED = 2};

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

  // pick-up a neighbor of c
  for (int i = 0; i < 6; i++) {
    p1 = c->getInterface((NeighborDirection::Direction)i);
    if (p1->connectedInterface) {
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

static Catoms2DMove* nextMove(Catoms2DBlock  *c) {
  P2PNetworkInterface *p2p = lastNeighborInDirection(c,ROTATION_DIRECTION);
  Catoms2DBlock* pivot = (Catoms2DBlock*)p2p->connectedInterface->hostBlock;
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

static bool pivotShouldMoveBefore(Catoms2DBlock *c, Catoms2DMove &mv,
				  int gradient[]) {
  Coordinate p1(c->position[0], c->position[2]);
  Coordinate p2 = getPosition(c,mv);
  
  bool pcm = canMove(mv.getPivot(),gradient);
  bool psmb = false;

  if (pcm) {
    Coordinate pivotP1(mv.getPivot()->position[0],
		       mv.getPivot()->position[2]);
    Catoms2DMove *pivotMv = nextMove(mv.getPivot());
    
    if (pivotMv != NULL) {
	Coordinate pivotP2 = getPosition(mv.getPivot(),*pivotMv);
	psmb = (!isInTarget(pivotP1) || 
		(isInTarget(pivotP1) && isInTarget(pivotP2)));
#if defined(STRATEGY_ONE)	
	//psmb = psmb && (pivotP1.y <= p1.y);
	if (mv.getPivot() != 
	    firstNeighborInDirection(c,ROTATION_DIRECTION)->connectedInterface->hostBlock) {
	  psmb = false;
	  }

#elif defined(STRATEGY_TWO)
	if (mv.getPivot() != 
	    lastNeighborInDirection(c,ROTATION_DIRECTION)->connectedInterface->hostBlock) {
	  psmb = false;
	}
#endif
    }
  }
  return psmb;
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
    states[i] = OUT_SHAPE;
    //Catoms2DBlock *c = (Catoms2DBlock*) world->getBlockById(i);
  }

  map<int, BuildingBlock*>::iterator it;
  for (it=world->getMap().begin() ; it != world->getMap().end(); ++it) {
    Catoms2DBlock *c = (Catoms2DBlock*) it->second;
    Coordinate p(c->position[0],c->position[2]);
    if (isInTarget(p)) {
      states[c->blockId] = IN_SHAPE;
     } else {
      states[c->blockId] = OUT_SHAPE;
    }
  }
  
  
  bfs = Tree::bfs(seed->blockId,gradient,world->getSize());

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
#if defined(STRATEGY_ONE) || defined (STRATEGY_TWO)
    map<int, BuildingBlock*>::iterator it;
    for (it=world->getMap().begin() ; it != world->getMap().end(); ++it) {

      c = (Catoms2DBlock*) it->second;
      if (c == seed) {
	continue;
      }

      if (canMove(c,gradient)) {
	//cout << "c satisfies gradient condition" << endl;
	//cout << "@" << c->blockId << " can physically move" << endl;
	Coordinate p1(c->position[0], c->position[2]);
	Catoms2DMove *mv = nextMove(c);
	if (mv != NULL) {
	  Coordinate p2 = getPosition(c,*mv);

	  bool psmb = pivotShouldMoveBefore(c,*mv,gradient);
	  if (!psmb && 
	      (!isInTarget(p1) || (isInTarget(p1) && isInTarget(p2)))) {
	    cout << c->blockId << " is moving from " << p1 << " to " 
		 << p2 << " using " << mv->getPivot()->blockId << " in direction " << mv->getDirection() << "..."; 
	    move(c,*mv);
	    //cout << c->blockId << " has " << c->nbNeighbors() << " neighbors" << endl; 
	    gradient[c->blockId] = UNDEFINED_GRADIENT;
	    updateGradient(c,gradient);
	    cout << " done"; //<< endl;
	    getchar();
	  } /*else {
	    cout << "hors figure ?" << endl;
	    }*/
	  delete mv;
	}// else { cout << " move == NULL" << endl;}
      }
    }
#elif defined(STRATEGY_THREE) 
    Coordinate best(INT_MAX,INT_MIN);
    map<int, BuildingBlock*>::iterator it;
    for (it=world->getMap().begin() ; it != world->getMap().end(); ++it) {
      
      c = (Catoms2DBlock*) it->second;
      if (c == seed) {
	continue;
      }

      if (canMove(c,gradient)) {
	Coordinate p1(c->position[0], c->position[2]);	  	
	Catoms2DMove *mv = nextMove(c);
	if (mv != NULL) {
	  Coordinate p2 = getPosition(c,*mv);
	  if (!isInTarget(p1) || (isInTarget(p1) && isInTarget(p2))) {
	    if (p1.y > best.y) {
	      best = p1;
	    } else if (p1.y == best.y) {
	      if (p1.x < best.x) {
		best = p1;
	      }
	    }
	  }
	  delete mv;
	}
      }
    }
    //cout << "best: " << best << endl;
    c = world->getGridPtr(best.x,0,best.y);
    Catoms2DMove *mv = nextMove(c);
    if (mv == NULL) { cout << "mv null" << endl;} 
    Coordinate p1(c->position[0], c->position[2]); 
    Coordinate p2 = getPosition(c,*mv);	
    cout << c->blockId << " is moving from " << p1 << " to " 
	 << p2 << " using " << mv->getPivot()->blockId << " in direction " << mv->getDirection() << "..."; 
    // mv should not be null (tested above)
    move(c,*mv);
    gradient[c->blockId] = UNDEFINED_GRADIENT;
    updateGradient(c,gradient);	  
    cout << " done"; //<< endl;
    getchar();
    delete mv;
#elif defined(STRATEGY_FOUR)
    // 4: node OUT_SHAPE on the perimeter closer from the node IN_SHAPE that has
    // lowest neighbor cell to fill.
    Coordinate lowest(INT_MIN,INT_MAX);
    cout << "computing lowest" << endl;
    map<int, BuildingBlock*>::iterator it;
    for (it=world->getMap().begin() ; it != world->getMap().end(); ++it) {
      
      c = (Catoms2DBlock*) it->second;
      Coordinate p1 = Coordinate(c->position[0],c->position[2]);
      if (states[c->blockId] != IN_SHAPE) {
	continue;
      }

      for (int i = 0; i < 6; i++) {
	P2PNetworkInterface *p2p = c->getInterface((NeighborDirection::Direction)i);
	if (!p2p->connectedInterface) {
	  Coordinate p2 = Map::getPosition(c,p1,p2p);
	  if (isInTarget(p2)) {
	    if (p2.y < lowest.y) {
	      lowest = p2;
	    } else if (p2.y == lowest.y) {
	      if (p2.x > lowest.x) {
		lowest = p2;
	      }
	    }
	  }
	}
      }
    }
    // we found the lowest cell to fill
    // now, we follow the perimeter to found the closest OUT_SHAPE catoms
    // (reverse order than the rotation direction)
    cout << lowest << endl;
    //Catoms2DMove::direction_t d = reverseDirection(ROTATION_DIRECTION);
    c = world->getGridPtr(lowest.x,0,lowest.y);
    P2PNetworkInterface *p2p = firstNeighborInDirection(c,ROTATION_DIRECTION);
    Catoms2DBlock *cc = (Catoms2DBlock*) p2p->connectedInterface->hostBlock;
#ifdef COLOR_DEBUG
    c->setColor(RED);
    cc->setColor(GREEN);
#endif
    /*bool willFillTheCell = false;
    while (!willFillTheCell) {
      if (canMove(cc,gradient)) {
	Coordinate p1(cc->position[0], cc->position[2]);	  	
	Catoms2DMove *mv = nextMove(c);
	if (mv != NULL) {
	  Coordinate p2 = getPosition(c,*mv);
	  if (!isInTarget(p1) || (isInTarget(p1) && isInTarget(p2))) {
	    
	  }
	  delete mv;
      }
      }*/
    getchar();
#endif
  }
  cout << "reconfiguration over in " << moves << " moves." << endl;
}
