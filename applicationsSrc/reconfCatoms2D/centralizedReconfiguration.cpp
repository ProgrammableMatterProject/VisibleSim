#include <iostream>
#include <climits>
#include "centralizedReconfiguration.h"
#include "bfs.h"
#include "robots/catoms2D/catoms2DWorld.h"
#include "coordinate.h"

#include "rotation2DEvents.h"
#include "map.h"

using namespace std;
using namespace Catoms2D;

#define COLOR_DEBUG
#define ROTATION_DIRECTION RelativeDirection::CW
#define UNDEFINED_GRADIENT -1

#define CONSECUTIVE_NEIGHBORS
#define NB_MAX_CONSECUTIVE_NEIGHBORS_TO_MOVE 3
//#define GRADIENT

enum state_t {MOVING = 0, UNMOVING = 1, STABLE = 2, HAS_TO_MOVE = 3, UNKNOWN = 4};

static int moves = 0;

static Coordinate getMapBottomRight() {
    Catoms2DWorld *world = Catoms2DWorld::getWorld();
    Catoms2DBlock  *c;
    Coordinate rb(INT_MIN,INT_MAX);

    map<bID, BuildingBlock*>::iterator it;
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
    Cell3DPosition gridSize = world->lattice->gridSize;
    for (int iy = 0; iy < gridSize[2]; iy++) {
        for (int ix = 0; ix < gridSize[0]; ix++) {
                Cell3DPosition c(p.x,0,p.y);
            if (BlockCode::target->isInTarget(c)) {
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
            Cell3DPosition v = c->getPosition((HLattice::Direction) i);
        Coordinate ci(v[0],v[2]);
        if (ci == p) {
            return true;
        }
    }
    return false;
}

static bool isOver() {
    Catoms2DWorld *world = Catoms2DWorld::getWorld();
    Cell3DPosition gridSize = world->lattice->gridSize;
    for (int iy = 0; iy < gridSize[2]; iy++) {
        for (int ix = 0; ix < gridSize[0]; ix++) {
                Cell3DPosition c(ix,0,iy);
            if (BlockCode::target->isInTarget(c)) {
                if (!world->lattice->getBlock(c)) {
                    return false;
                }
            }
        }
    }
    return true;
}

static P2PNetworkInterface*
nextInterface(Catoms2DBlock *c, RelativeDirection::Direction d, P2PNetworkInterface *p2p) {
    int p2pDirection = c->getDirection(p2p);
    if (d == RelativeDirection::CCW) {
        if (p2pDirection == HLattice::Direction::BottomRight) {
                p2pDirection = HLattice::Direction::Right;
        } else {
            p2pDirection++;
        }
    } else if (d == RelativeDirection::CW) {
            if (p2pDirection == HLattice::Direction::Right) {
            p2pDirection = HLattice::Direction::BottomRight;
        } else {
            p2pDirection--;
        }
    }
    return c->getInterface(p2pDirection);
}
/*
  static P2PNetworkInterface *extremeNeighborInDirection(Catoms2DBlock *c, RelativeDirection::Direction d) {
  P2PNetworkInterface *p1 = NULL, *p2 = NULL;

  if (c->nbNeighbors(false) == 0) {
  return NULL;
  }

  int cn = c->nbConsecutiveNeighbors(false);

  // pick-up a neighbor of c in the longest sequence of consecutive neighbors
  for (int i = 0; i < 6; i++) {
  int n = 0;

  p1 = c->getInterface((NeighborDirection::Direction)i);
  if (!c->hasANeighbor(p1,false)) {
  continue;
  }
  n = 1;
  p2 = p1;

  while (n != cn) {
  p2 = nextInterface(c,d,p2);
  if(!c->hasANeighbor(p2,false)) {
  break;
  }
  n++;
  }
  if (n == cn) {
  break;
  }
  }

  if (c->nbNeighbors(false) == 1) {
  return p1;
  }

  p2 = p1;
  while (false) {
  p2 = nextInterface(c, d, p2);
  if (!c->hasANeighbor(p2,false)) {
  return p1;
  }
  p1 = p2;
  }
  return NULL;
  }*/

static P2PNetworkInterface *extremeNeighborInDirection(Catoms2DBlock *c, RelativeDirection::Direction d) {
    P2PNetworkInterface *p1 = NULL, *p2 = NULL;

    if (c->nbNeighbors(true) == 0) {
        return NULL;
    }

    int cn = c->nbConsecutiveNeighbors(true);

    // pick-up a neighbor of c in the longest sequence of consecutive neighbors
    for (int i = 0; i < 6; i++) {
        int n = 0;

        p1 = c->getInterface(i);
        if (!c->hasANeighbor(p1,false)) {
            continue;
        }
        n = 1;
        p2 = p1;

        while (n != cn) {
            p2 = nextInterface(c,d,p2);
            if(!c->hasANeighbor(p2,true)) {
                break;
            }
            n++;
        }
        if (n == cn) {
            break;
        }
    }

    if (c->nbNeighbors(true) == 1) {
        return p1;
    }

    p2 = p1;
    while (true) {
        p2 = nextInterface(c, d, p2);
        if (!c->hasANeighbor(p2,true)) {
            return p1;
        }
        if (c->hasANeighbor(p2,false)) {
            p1 = p2;
        }
    }
    return NULL;
}

static  RelativeDirection::Direction reverseDirection(RelativeDirection::Direction d) {
    if (d == RelativeDirection::CCW) {
        return RelativeDirection::CW;
    } else if (d == RelativeDirection::CW) {
        return RelativeDirection::CCW;
    }
    return RelativeDirection::CW; // default
}

static P2PNetworkInterface *lastNeighborInDirection(Catoms2DBlock *c, RelativeDirection::Direction d) {
    // the last one is the first one in the opposite direction
    return extremeNeighborInDirection(c,reverseDirection(d));
}

static P2PNetworkInterface *firstNeighborInDirection(Catoms2DBlock *c, RelativeDirection::Direction d) {
    return extremeNeighborInDirection(c,d);
}

static P2PNetworkInterface* previousInterfacePerimeter(Catoms2DBlock  *c) {
    return firstNeighborInDirection(c,ROTATION_DIRECTION);
}

static P2PNetworkInterface* nextInterfacePerimeter(Catoms2DBlock  *c) {
    return lastNeighborInDirection(c,ROTATION_DIRECTION);
}

static Catoms2DBlock* nextCatomPerimeter(Catoms2DBlock  *c) {
    if (!nextInterfacePerimeter(c)->connectedInterface) {
        cout << "NULL INTERFACE!" << endl;
    }
    return (Catoms2DBlock*)nextInterfacePerimeter(c)->connectedInterface->hostBlock;
}

static Catoms2DBlock* previousCatomPerimeter(Catoms2DBlock  *c) {
    return (Catoms2DBlock*)previousInterfacePerimeter(c)->connectedInterface->hostBlock;
}

static Rotation2DMove* nextMove(Catoms2DBlock  *c) {
    Catoms2DBlock* pivot = nextCatomPerimeter(c);
    Rotation2DMove m(pivot,ROTATION_DIRECTION);
    if (c->canMove(m)) {
        //cout << c->blockId << " can move arround " << pivot->blockId << endl;
        return new Rotation2DMove(m);
    }
    return NULL;
}

static Coordinate getPosition(Catoms2DBlock* c, Rotation2DMove &m) {
    P2PNetworkInterface *p2p = m.getPivot()->getP2PNetworkInterfaceByBlockRef(c);
    Coordinate position(m.getPivot()->position[0], m.getPivot()->position[2]);
    p2p = nextInterface(m.getPivot(),m.getDirection(),p2p);
    return Map::getPosition(m.getPivot(),position,p2p);
}

static bool isInTarget(Coordinate &p) {
  Cell3DPosition c(p.x,0,p.y);
  return (BlockCode::target->isInTarget(c));
}

/*
  P2PNetworkInterface *nextPerimeterInterface2(Catoms2DBlock* c, Catoms2DBlock *ignore) {
  // enum Direction {Right = 0, TopRight = 1, TopLeft = 2, Left = 3, BottomLeft = 4, BottomRight = 5};
  NeighborDirection::Direction directions[] = {NeighborDirection::TopRight, NeighborDirection::Right, NeighborDirection::BottomRight, NeighborDirection::BottomLeft, NeighborDirection::Left, NeighborDirection::TopLeft};

  for (int i = 0; i < 6; i++) {
  P2PNetworkInterface *p = c->getInterface(directions[i]);
  if (p->connectedInterface && (p->connectedInterface->hostBlock != ignore)) {
  return p;
  }
  }
  return NULL;
  }

  Catoms2DBlock *nextPerimeterNeighbor2(Catoms2DBlock* c, Catoms2DBlock *ignore) {
  P2PNetworkInterface *p = nextPerimeterInterface2(c,ignore);

  if (p != NULL) {
  return (Catoms2DBlock*) p->connectedInterface->hostBlock;
  }

  p = nextPerimeterInterface2(c,NULL);

  if (p != NULL) {
  return (Catoms2DBlock*) p->connectedInterface->hostBlock;
  }

  return NULL;

  }*/

static bool canMove(Catoms2DBlock *c, Rotation2DMove &m, state_t states[]) {
    Coordinate g = getPosition(c,m);
    Catoms2DBlock *next = m.getPivot();
    Catoms2DBlock *previous = c;
    Catoms2DBlock *pre_previous = NULL;

    //int ids[5] = {-1,-1,-1,-1,-1};
    int i = 0;

    cout << "canMove " << c->blockId << " around " << next->blockId << "? ";


    if (!isNeighbor(next,g)) {
        cerr << "error pivot is not a neighbor" << endl;
    }

    while (isNeighbor(next,g)) {

        //if (pre_previous != next) {
        i++;
        // }

        if (states[next->blockId] == UNKNOWN) {
            cerr << ": initialization error!" << endl;
            return false;
        }

        if (states[next->blockId] == MOVING) {
            cout << ": " << c->blockId << " can't move because of " << next->blockId << endl;
            return false;
        }

        if (i == 4) {
            cout << endl;
            cout << previous->blockId << " " << next->blockId << endl;
            cout << ": " << c->blockId << " can't because i == 4 " << next->blockId << endl;
            //if (states[next->blockId] != MOVING) {
            cout << next->blockId  << " has to move!" << endl;
            states[next->blockId] = HAS_TO_MOVE;
            //}
            return false;
        }


        //Catoms2DBlock *n = NULL;
        bool mustStop = true;
        cout << endl << "neighbors of next (" << next->blockId << ")" << endl;
        for (int i = 0; i < 6; i++) {
            P2PNetworkInterface *p = next->getInterface(i);
            if (p->connectedInterface) {
                Catoms2DBlock *n = (Catoms2DBlock*)p->connectedInterface->hostBlock;
                cout << " " << n->blockId;
                if ((n != previous) && isNeighbor(n,g)) {
                    previous = next;
                    next = n;
                    mustStop = false;
                    cout << "selected neighbor: " << next->blockId << endl;
                    break;
                }
            }
        }
        cout << endl;
        if (mustStop) {
            cerr << "next == NULL" << endl;
            return true;
        }
        cout << ", next: " << next->blockId;
    }

    /*int cnt = 0;
      for (it=world->getMap().begin() ; it != world->getMap().end(); ++it) {

      neighbor = (Catoms2DBlock*) it->second;

      if (neighbor == c) {
      continue;
      }

      if (isNeighbor(neighbor,g)) {
      cnt++;
      if (cnt  == 4) {
      cout << endl;
      cout << previous->blockId << " " << neighbor->blockId << endl;
      cout << ": " << c->blockId << " can't because i == 4 " << neighbor->blockId << endl;
      //if (states[next->blockId] != MOVING) {
      cout << neighbor->blockId  << " has to move!" << endl;
      states[neighbor->blockId] = HAS_TO_MOVE;
      //}
      return false;
      }

      if (states[neighbor->blockId] == UNKNOWN) {
      cerr << ": initialization error!" << endl;
      return false;
      }

      if (states[neighbor->blockId] == MOVING) {
      cout << ": " << c->blockId << " can't move because of " << neighbor->blockId << endl;
      return false;
      }
      }
      }*/
    /* while (isNeighbor(next,g)) {


       if (pre_previous != next) {
       i++;
       }

       if (states[next->blockId] == UNKNOWN) {
       cerr << ": initialization error!" << endl;
       return false;
       }

       if (states[next->blockId] == MOVING) {
       cout << ": " << c->blockId << " can't move because of " << next->blockId << endl;
       return false;
       }

       if (i == 4) {
       cout << endl;
       cout << previous->blockId << " " << next->blockId << endl;
       cout << ": " << c->blockId << " can't because i == 4 " << next->blockId << endl;
       //if (states[next->blockId] != MOVING) {
       cout << next->blockId  << " has to move!" << endl;
       states[next->blockId] = HAS_TO_MOVE;
       //}
       return false;
       }

       if (pre_previous == next) {
       return true;
       }

       for (int i = 0; i < 6; i++) {
       P2PNetworkInterface *p = next->getInterface((NeighborDirection::Direction)i);
       if (p->connectedInterface) {
       Catoms2DBlock *n = (Catoms2DBlock*)p->connectedInterface->hostBlock;
       if ((n!=c) && (n != nextCatomPerimeter(next)) && (n != previousCatomPerimeter(next)) && isNeighbor(n,g) && (states[n->blockId] == MOVING)) {
       cout << endl;
       return false;
       }
       }
       }*

       Catoms2DBlock *tmp = next;
       next = nextCatomPerimeter(next);
       pre_previous = previous;
       previous = tmp;
       //next =  nextPerimeterNeighbor2(next,previous);

       if (next == c) {
       cerr << "next == c" << endl;
       return true;
       }
       cout << ", next: " << next->blockId;
       }*/
    cout << "ok" << endl;
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

static void updateState(state_t states[], int gradient[]) {
    Catoms2DWorld *world = Catoms2DWorld::getWorld();
    Catoms2DBlock *c;

    map<bID, BuildingBlock*>::iterator it;
    for (it=world->getMap().begin() ; it != world->getMap().end(); ++it) {

        c = (Catoms2DBlock*) it->second;

        if (states[c->blockId] == STABLE) {
            c->setColor(GREEN);
            continue;
        }

        if (states[c->blockId] == HAS_TO_MOVE) {
            c->setColor(ORANGE);
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
                Coordinate p1(c->position[0], c->position[2]);
                Rotation2DMove *mv = nextMove(c);

                if (mv == NULL && isInTarget(p1)) {
                    states[c->blockId] = STABLE;
                }

                if (mv != NULL) {
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
}

static void move(Catoms2DBlock* c, Rotation2DMove &m) {
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
    world->lattice->insert(c, Cell3DPosition(p.x,0,p.y));
    c->setPosition(Cell3DPosition(p.x,0,p.y));
    world->updateGlData(c,world->lattice->gridToWorldPosition(c->position),c->angle);
    // connect
    world->connectBlock(c);
    moves++;
}

static void updateGradient(Catoms2DBlock *c, int gradient[]) {
// can algorithmically move (gradient higher or equal than all neighbors)
    int id1 = c->blockId;
    int minGradient = INT_MAX;

    for (int i = 0; i < 6; i++) {
        P2PNetworkInterface *p2p = c->getInterface(i);
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
    seed = (Catoms2DBlock *)world->lattice->getBlock(Cell3DPosition(mapBottomRight.x,0,mapBottomRight.y));
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

    updateState(states,gradient);
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

        map<bID, BuildingBlock*>::iterator it;
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
                Rotation2DMove *mv = nextMove(c);
                if (mv != NULL) {
                    Coordinate p1(c->position[0], c->position[2]);
                    Coordinate p2 = getPosition(c,*mv);

                    if (!canMove(c,*mv,states)) {
                        continue;
                    }

                    if ((!isInTarget(p1) || (states[c->blockId] == HAS_TO_MOVE) || (isInTarget(p1) && isInTarget(p2) && (p2.y <= p1.y)))) {
                        states[c->blockId] = MOVING;
                        cout << c->blockId << " is moving from " << p1 << " to "
                             << p2 << " using " << mv->getPivot()->blockId << " in direction " << mv->getDirection();
                        move(c,*mv);
                        //cout << c->blockId << " has " << c->nbNeighbors() << " neighbors" << endl;
                        gradient[c->blockId] = UNDEFINED_GRADIENT;
                        updateGradient(c,gradient);
                        updateState(states,gradient);
                        cout << " done" << endl;

                        //Rotation2DMove counterMV(mv->getPivot(),reverseDirection(mv->getDirection()));
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
