/*
 * c2sr.cpp
 *
 *  Created on: 27 nov 2015
 *      Author: Andre Naz
 */

#include <set>
#include <iostream>
#include <cstring>
#include <memory>
#include <map>

#include "catoms2DWorld.h"
#include "scheduler.h"
#include "events.h"
#include "c2sr.h"
#include "c2srMsg.h"
#include "catom2D1BlockCode.h"

//#define RECONFIGURATION_DEBUG

//#define RECONFIGURATION_NEIGHBORHOOD_DEBUG
//#define RECONFIGURATION_MOVES_DEBUG
//#define C2SR_MSG_DEBUG
//#define RECONFIGURATION_CLEARANCE_DEBUG

#define RECONFIGURATION_WITH_COLOR

#define RECONFIGURATION_CHECKSPACE_DEBUG
#define CHECK_SPACE

//#define RECONFIGURATION_PARALLELISM_COLOR

using namespace std;
using namespace Catoms2D;

list<Catoms2DBlock*> C2SR::movingModules;

#define ROTATION_DIRECTION RelativeDirection::CW
#define OPPOSITE_ROTATION_DIRECTION RelativeDirection::getOpposite(ROTATION_DIRECTION)

#define STRATEGY_TWO

#define MY_CERR cerr << "@" << catom->blockId << " (" << map->position << ")"

/***** ClearanceRequest Class *****/
ClearanceRequest::ClearanceRequest() {}

ClearanceRequest::ClearanceRequest(Coordinate &s, Coordinate &d, int c) {
  src = s;
  dest = d;
  cnt = c;
}

ClearanceRequest::ClearanceRequest(const ClearanceRequest &cr) {
  src = cr.src;
  dest = cr.dest;
  cnt = cr.cnt;
}

ClearanceRequest::~ClearanceRequest() {}

string ClearanceRequest::toString() {
  string s = "(src=" + src.toString() + ",dest=" + dest.toString() + ",cnt=" + to_string(cnt);
  return s;
}

/***** Clearance Class *****/
Clearance::Clearance() {}

Clearance::Clearance(Coordinate &s, Coordinate &d) {
  src = s;
  dest = d;
}

Clearance::Clearance(const Clearance &c) {
  src = c.src;
  dest = c.dest;
}

Clearance::~Clearance() {}

string Clearance::toString() {
  string s = "(src=" + src.toString() + ",dest=" + dest.toString();
  return s;
}

/***** C2SR Class *****/

C2SR::C2SR(Catoms2DBlock *c, Map *m): map(m) {
  catom = c;
  setState(UNKNOWN);
  started = false;
}

C2SR::~C2SR() {}

std::string C2SR::toString(C2SRState_t s) {
  switch(s) {
  case NOT_SET:
    return "NOT_SET";
    break;
  case UNKNOWN:
    return "UNKNOWN";
    break;
  case BLOCKED:
    return "BLOCKED";
    break;
  case WAITING:
    return "WAITING";
    break;
  case MOVING:
    return "MOVING";
    break;
  case GOAL:
    return "GOAL";
    break;
  default:
    return "Error: UNKNWON STATE!";
  }
}

void C2SR::handleStopMovingEvent() {

  // This module just stops to roll

  // Update Position
  map->setPosition(currentClearance.dest);

  // Verif
  assert(isFree());

  setState(WAITING);

  // Verif2
#ifdef CHECK_SPACE
  removeMovings(catom);
  assert(checkSpace());
#endif

  // send end of move to ?
  Neighbor n = map->getBorder(ROTATION_DIRECTION);
  Message *m = new C2SREndMoveMsg(currentClearance,1);
  send(m,n.interface);

  if (checkConvergence()) {
    setState(GOAL);
  } else if (isInStream()) {
    requestClearance();
  }
}


void C2SR::printNeighbors() {
  P2PNetworkInterface* p2p;
  MY_CERR << " neighbors: ";
  for (int j = 0; j < 6; j++) {
    p2p = catom->getInterface(j);
    if(p2p->connectedInterface) {
      cerr << p2p->connectedInterface->hostBlock->blockId << " ";
    }
  }
  cerr << endl;
}

void C2SR::handle(MessagePtr m) {
  C2SRMsg_ptr rm = std::static_pointer_cast<C2SRMsg>(m);
  P2PNetworkInterface *recv = m->destinationInterface;
  assert(recv->connectedInterface);

#ifdef C2SR_MSG_DEBUG
  P2PNetworkInterface *from = m->sourceInterface;
  int fromId = recv->connectedInterface->hostBlock->blockId;
  MY_CERR << " " << rm->toString() << " from " << fromId << endl;
#endif

  //MY_CERR << "hop count: " << rm->hopCounter << endl;
  //MY_CERR << "size: " << rm->size() << endl;

#ifdef CHECK_SPACE
  assert(checkSpace());
#endif

  if (!started) {
    start();
  }

  switch(rm->subtype) {

  case C2SRMsg::CLEARANCE_REQUEST: {
    C2SRClearanceRequestMsg_ptr crm = std::static_pointer_cast<C2SRClearanceRequestMsg>(m);

    bool delayed = false;
    bool hasNext = false;

    Neighbor next = map->getNeighbor(ROTATION_DIRECTION,crm->request.dest);

#ifdef C2SR_MSG_DEBUG
    printMoving();
#endif

    if (next.interface != recv && Map::areNeighbors(crm->request.dest,next.position)) {
      hasNext = true;
    }

    // Rules
    if (state == WAITING) {
      // false can't move, this module will move first
      // previous module will store the delayed ClearanceRequest
      delayed = true;
    }

    // movings!
    // if a module is moving in a cell neighbor to request.dest (src should be already managed)
    if (isNeighborToMoving(crm->request.dest)) {

#ifdef RECONFIGURATION_CLEARANCE_DEBUG
      MY_CERR <<crm->request.toString()  << " delayed here!" << endl;
#endif
      // delayed but store the request here
      insertPendingRequest(crm->request);
      break;
    }

    if (state == BLOCKED || state == GOAL) {
      crm->request.cnt++;
      if (crm->request.cnt == 4) {
    // wait for this module to move,
    // send delayed ClearanceRequest to previous
    crm->request.cnt--;
    delayed = true;
      }
    }

    if (delayed) {
      // clearance rejected
      Message *m = new C2SRDelayedClearanceRequestMsg(crm->request,1);
      send(m,recv);
    } else if (hasNext) {
      // check next neighbor
      Message *m = new C2SRClearanceRequestMsg(crm->request, rm->hopCounter+1);
#ifdef C2SR_MSG_DEBUG
      MY_CERR << "CLEARANCE_REQUEST forwarded to : " << next.interface->connectedInterface->hostBlock->blockId << endl;
#endif
      send(m, next.interface);
    } else {
      // clearance granted
      Clearance c(crm->request.src,crm->request.dest);
      insertMoving(c.dest);
      forwardClearance(c,NULL,1);
    }
  }
    break;
  case C2SRMsg::CLEARANCE: {
    C2SRClearanceMsg_ptr cm = std::static_pointer_cast<C2SRClearanceMsg>(m);

    // Find the right condition!
    if (map->getPosition() == cm->clearance.src) {
      // move
      currentClearance = cm->clearance;
      Message *m = new C2SRStartMoveMsg(1);
      send(m,recv);
    } else {
      forwardClearance(cm->clearance, recv,rm->hopCounter+1);
    }
  }
    break;
  case C2SRMsg::DELAYED_CLEARANCE_REQUEST: {
    C2SRDelayedClearanceRequestMsg_ptr dcrm = std::static_pointer_cast<C2SRDelayedClearanceRequestMsg>(m);

    if (dcrm->request.src != map->getPosition()) {
#ifdef RECONFIGURATION_CLEARANCE_DEBUG
      MY_CERR << dcrm->request.toString()  << " delayed!" << endl;
#endif
      insertPendingRequest(dcrm->request);
    }
  }
    break;
  case C2SRMsg::START_TO_MOVE: {
    Message *m = new C2SRStartMoveAckMsg(1);
    send(m,recv);
  }
    break;
  case C2SRMsg::START_TO_MOVE_ACK: {
    move(currentClearance);
  }
    break;
  case C2SRMsg::END_OF_MOVE: {
    C2SREndMoveMsg_ptr emm = std::static_pointer_cast<C2SREndMoveMsg>(m);

    // Remove movings and forward end_of_move
    removeMoving(emm->clearance.dest);
    removeMoving(emm->clearance.src);

    forwardEndMove(emm->clearance,recv,rm->hopCounter+1);

    // and now:
    if (isInStream()) {
      setState(WAITING);
      assert(pendingRequests.size() == 0);

      requestClearance();
    } else {

      if (isAPendingRequestDestNeighborWith(emm->clearance.src)) {
    ClearanceRequest cr = getPendingRequestDestNeighborWith(emm->clearance.src);
    Neighbor next = map->getNeighbor(ROTATION_DIRECTION,cr.dest);
    if (Map::areNeighbors(next.position,cr.dest)) {
      Message *m = new C2SRClearanceRequestMsg(cr,1);
      send(m,next.interface);
    } else { // clearance granted!
      Clearance c(cr.src,cr.dest);
      insertMoving(c.dest);
      forwardClearance(c,NULL,1);
    }
      }

    }
  }
    break;
  default:
    cerr << "unknown c2sr message type" << endl;
  }
}

void C2SR::move(Clearance &c) {
  Neighbor pivot = map->getBorder(ROTATION_DIRECTION);
  Coordinate dest = getPositionAfterRotationAround(pivot);
  //Coordinate &src = map->getPosition();

  Catoms2DBlock* piv= (Catoms2DBlock*)pivot.interface->connectedInterface->hostBlock;
  Catoms2DRotationMove m(piv,ROTATION_DIRECTION);

  assert(isFree());
  assert(dest == c.dest); // otherwise, it's not the good clearance!

#ifdef CHECK_SPACE
  insertMovings(catom);
  assert(checkSpace());
#endif

  currentClearance = c;

#ifdef RECONFIGURATION_MOVES_DEBUG
  MY_CERR << " moving " << currentClearance.toString() << endl;
#endif

  setState(MOVING);

  // start to move around the pivot
  catom->startMove(m);
}

Coordinate C2SR::getPositionAfterRotationAround(Neighbor &pivot) {
  P2PNetworkInterface *cellInt = catom->getNextInterface(OPPOSITE_ROTATION_DIRECTION, pivot.interface, false);
  Coordinate c = map->getPosition(cellInt);
  return c;
}

void C2SR::requestClearance() {
  Neighbor pivot = map->getBorder(ROTATION_DIRECTION);
  Coordinate dest = getPositionAfterRotationAround(pivot);
  Coordinate &src = map->getPosition();

  Neighbor b = map->getBorder(ROTATION_DIRECTION);
  ClearanceRequest cr(src,dest,0);
  Message *m = new C2SRClearanceRequestMsg(cr,1);
  send(m,b.interface);
}

bool C2SR::isInStream() {

  if (!isFree()) {
    return false;
  }

  Neighbor pivot = map->getBorder(ROTATION_DIRECTION);
  Coordinate dest = getPositionAfterRotationAround(pivot);
  Coordinate &src = map->getPosition();

  //if (!Map::isInTarget(src) && (src.y == 0)) { // ground
  if (!Map::isInTarget(src) && (pivot.position.y == 0)) { // ground
    return true;
  } else if (!Map::isInTarget(src) && (dest.y <= src.y)) { // descent over I
    return true;
  } else if (!Map::isInTarget(src) && Map::isInTarget(pivot.position)) { //
    return true;
  } else if (Map::isInTarget(src) && Map::isInTarget(dest) && (dest.y <= src.y)) {
    return true;
  } else {
    return false;
  }
}

bool C2SR::checkConvergence() {
  Neighbor pivot = map->getBorder(ROTATION_DIRECTION);
  Coordinate dest = getPositionAfterRotationAround(pivot);
  Coordinate &src = map->getPosition();

  if ((Map::isInTarget(src) && !Map::isInTarget(dest)) ||
      (Map::isInTarget(src) && Map::isInTarget(dest) && (dest.y > src.y))) {
    return true;
  }
  return false;
}

bool C2SR::isFree() {
  int n = catom->nbNeighbors(true);
  int nc = catom->nbConsecutiveNeighbors(true);
  return ((n == nc) && (n <= 3) && (movings.size() == 0));
}

void C2SR::start() {

  if (!started) {

#ifdef RECONFIGURATION_NEIGHBORHOOD_DEBUG
    printNeighbors();
#endif

    started = true;
    if (Map::isInTarget(map->position)) { // seed
      hasConverged();
    } else if (isInStream()) {
      setState(WAITING);
      requestClearance();
    } else {
      setState(BLOCKED);
    }
  }
#ifdef RECONFIGURATION_DEBUG
  MY_CERR << " initial state " << toString(state) << endl;
#endif
}


bool C2SR::isAPendingRequestDestNeighborWith(Coordinate &p) {
  for (list<ClearanceRequest>::iterator it = pendingRequests.begin(); it != pendingRequests.end(); it++) {
    if (Map::areNeighbors(p,it->dest)) {
      return true;
    }
  }
  return false;
}

ClearanceRequest C2SR::getPendingRequestDestNeighborWith(Coordinate &p) {
  assert(isAPendingRequestDestNeighborWith(p));

  ClearanceRequest cr;
  for (list<ClearanceRequest>::iterator it = pendingRequests.begin(); it != pendingRequests.end(); it++) {
    if (Map::areNeighbors(p,it->dest)) {
      cr = *it;
      pendingRequests.erase(it);
      break;
    }
  }
  return cr;
}

void C2SR::insertPendingRequest(ClearanceRequest &pr) {
  pendingRequests.push_back(pr);
  assert(pendingRequests.size() < 3);
}

void C2SR::printPendingRequest() {
  MY_CERR << "PendingRequest:";
  for (list<ClearanceRequest>::iterator it = pendingRequests.begin(); it != pendingRequests.end(); it++) {
    cerr << " " << it->toString();
  }
  cerr << endl;
}

void C2SR::insertMoving(Coordinate &c) {
#ifdef RECONFIGURATION_CLEARANCE_DEBUG
  MY_CERR << "insert moving: " << c << endl;
#endif
  movings.push_back(c);
}

bool C2SR::isMoving(Coordinate &c) {
  list<Coordinate>::iterator it;
  for (it = movings.begin(); it != movings.end(); it++) {
    if (*it == c) {
      return true;
    }
  }
  return false;
}

bool C2SR::isNeighborToMoving(Coordinate &c) {
  list<Coordinate>::iterator it;
  for (it = movings.begin(); it != movings.end(); it++) {
    if (Map::areNeighbors(*it,c)) {
      return true;
    }
  }
  return false;
}

void C2SR::removeMoving(Coordinate &c) {
  list<Coordinate>::iterator it;
  for (it = movings.begin(); it != movings.end(); it++) {
    if (*it == c) {
      movings.erase(it);
      break;
    }
  }
}

void C2SR::printMoving() {
  MY_CERR << "Moving:";
  for (list<Coordinate>::iterator it = movings.begin(); it != movings.end(); it++) {
    cerr << " " << *it;
  }
  cerr << endl;
}

void C2SR::send(Message *m, P2PNetworkInterface *i) {
  assert(i);
  assert(i->connectedInterface);
  assert(m);

  i->send(m);
}

void C2SR::forwardClearance(Clearance &c, P2PNetworkInterface *recv, unsigned int h) {
  assert(Map::areNeighbors(map->getPosition(),c.src) || Map::areNeighbors(map->getPosition(),c.dest));

  Message *m = new C2SRClearanceMsg(c,h);

  if (Map::areNeighbors(map->getPosition(),c.src)) {
    Neighbor n = map->getNeighbor(OPPOSITE_ROTATION_DIRECTION,c.src);
    if (n.interface != recv && Map::areNeighbors(c.src,n.position)) {
      send(m,n.interface);
    } else {
      insertMoving(c.src);
      P2PNetworkInterface *p2p = map->getInterface(c.src);
      send(m,p2p);
    }
  } else if (Map::areNeighbors(map->getPosition(),c.dest)) {
    Neighbor n = map->getNeighbor(OPPOSITE_ROTATION_DIRECTION,c.dest);
    assert(Map::areNeighbors(n.position,c.dest));
    assert(n.interface != recv);
    send(m,n.interface);
  }
}

void C2SR::forwardEndMove(Clearance &c, P2PNetworkInterface *recv, unsigned int h) {
  assert(Map::areNeighbors(map->getPosition(),c.src) || Map::areNeighbors(map->getPosition(),c.dest));

  if (Map::areNeighbors(map->getPosition(),c.src)) {
    Neighbor n = map->getNeighbor(OPPOSITE_ROTATION_DIRECTION,c.src);
    if (n.interface != recv && Map::areNeighbors(c.src,n.position)) {
      Message *m = new C2SREndMoveMsg(c,h);
      send(m,n.interface);
    }
  } else if (Map::areNeighbors(map->getPosition(),c.dest)) {
    Message *m = new C2SREndMoveMsg(c,h);
    Neighbor n = map->getNeighbor(OPPOSITE_ROTATION_DIRECTION,c.dest);
    assert(Map::areNeighbors(n.position,c.dest));
    assert(n.interface != recv);
    send(m,n.interface);
  }
}

void C2SR::insertMovings(Catoms2D::Catoms2DBlock *c) {
  movingModules.push_back(c);
}

void C2SR::removeMovings(Catoms2D::Catoms2DBlock *c) {
  list<Catoms2DBlock*>::iterator it;
  for (it = movingModules.begin(); it != movingModules.end(); it++) {
    if (*it == c) {
      movingModules.erase(it);
      break;
    }
  }
}

#define CHECKSPACE_V2

bool C2SR::checkSpace() {
#ifdef CHECKSPACE_V1
  Catoms2DBlock *m1, *m2;

  double epsilon = 0.001;
  double limit = 3*CATOMS_RADIUS;
  std::list<Catoms2DBlock*>::iterator it1;
  std::list<Catoms2DBlock*>::iterator it2;

  for(it1 = movingModules.begin(); it1 != movingModules.end(); ++it1) {
    for(it2 = movingModules.begin(); it2 != movingModules.end(); ++it2) {
     if (*it1 == *it2) { continue;}
     m1 = *it1;
     m2 = *it2;
     Vector3D p1 = m1->ptrGlBlock->getPosition();
     Vector3D p2 = m2->ptrGlBlock->getPosition();
     double d = distance(p1,p2) + epsilon;

     if (d < limit) {
#ifdef RECONFIGURATION_CHECKSPACE_DEBUG
       Catoms2D1BlockCode *bc1, *bc2;
       bc1 = (Catoms2D1BlockCode*) m1->blockCode;
       bc2 = (Catoms2D1BlockCode*) m2->blockCode;
       cout.precision(17);
       MY_CERR << "SPACE Condition violated by modules "
           << m1->blockId << "(" << bc1->c2sr->currentClearance.toString() << ")"
           << " and "
           << m2->blockId << "(" << bc2->c2sr->currentClearance.toString() << ")"
           << " at distance " << fixed << d << " vs " << fixed << limit
           << endl;
       getchar();
#endif
        return false;
     }
    }
  }
  return true;
#endif

#ifdef CHECKSPACE_V2
  Catoms2DBlock *m1, *m2;
  Catoms2D1BlockCode *bc1, *bc2;
  C2SR *r1, *r2;

  std::list<Catoms2DBlock*>::iterator it1;
  std::list<Catoms2DBlock*>::iterator it2;

  for(it1 = movingModules.begin(); it1 != movingModules.end(); ++it1) {
    m1 = *it1;
    bc1 = (Catoms2D1BlockCode*) m1->blockCode;
    r1 = bc1->c2sr;
    vector<Coordinate> safetyZone = r1->getSafetyZone();

    assert(safetyZone.size() > 0);

    for(it2 = movingModules.begin(); it2 != movingModules.end(); ++it2) {
      if (*it1 == *it2) { continue;}

      m2 = *it2;
      bc2 = (Catoms2D1BlockCode*) m2->blockCode;
      r2 = bc2->c2sr;

      for (std::vector<Coordinate>::iterator it = safetyZone.begin(); it != safetyZone.end();
       ++it) {
    if ( (r2->currentClearance.src == *it) ||
         (r2->currentClearance.dest == *it)) {
#ifdef RECONFIGURATION_CHECKSPACE_DEBUG
      Vector3D p1 = m1->ptrGlBlock->getPosition();
      Vector3D p2 = m2->ptrGlBlock->getPosition();
      cout.precision(17);
      MY_CERR << "SPACE Condition violated by modules "
          << m1->blockId << "(" << r1->currentClearance.toString() << ")"
          << " and "
          << m2->blockId << "(" << r2->currentClearance.toString() << ")"
          << " at distance " << fixed << distance(p1,p2)
          << endl;
      getchar();
#endif
      return false;
    }
      }
    }
  }
  return true;
#endif

}

double C2SR::distance(Vector3D &p1, Vector3D &p2) {
  Vector3D t = p1 - p2;
  double d = t.norme();
  return d;
}

vector<Coordinate> C2SR::getSafetyZone() {
  Lattice *l = BaseSimulator::getWorld()->lattice;
  Coordinate srcC = currentClearance.src;
  Cell3DPosition srcP (srcC.x,0,srcC.y);
  vector<Cell3DPosition> srcNeighborhood = l->getNeighborhood(srcP);
  Coordinate destC = currentClearance.dest;
  Cell3DPosition destP(destC.x,0,destC.y);
  vector<Cell3DPosition> destNeighborhood = l->getNeighborhood(destP);
  vector<Coordinate> safetyZone;
  vector<Cell3DPosition> safetyZoneP = srcNeighborhood;

  safetyZoneP.insert(safetyZoneP.end(), destNeighborhood.begin(), destNeighborhood.end());

  for (std::vector<Cell3DPosition>::iterator it = safetyZoneP.begin();
       it != safetyZoneP.end(); ++it) {
    safetyZone.push_back(Coordinate((*it)[0],(*it)[2]));
  }
  return safetyZone;
}

bool C2SR::isDone() {
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

void C2SR::hasConverged() {
  setState(GOAL);
}

void C2SR::setState(C2SRState_t s) {
  state = s;

#ifdef RECONFIGURATION_WITH_COLOR
  if (state == GOAL) {
    Coordinate p = map->getPosition();
    assert(Map::isInTarget(p));
    Cell3DPosition c(p.x,0,p.y);
    catom->setColor(BlockCode::target->getTargetColor(c));
  }
#endif

#ifdef RECONFIGURATION_PARALLELISM_COLOR
  if (s == UNKNOWN) return;

  switch(s) {
  case GOAL:
    catom->setColor(GREEN);
    break;
  case WAITING:
    catom->setColor(YELLOW);
    break;
  case MOVING:
    catom->setColor(RED);
    break;
  case BLOCKED:
  case UNKNOWN:
    catom->setColor(GREY);
    break;
  default:
    ;;
  }
#endif
}
