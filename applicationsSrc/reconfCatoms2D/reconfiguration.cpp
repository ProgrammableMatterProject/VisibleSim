/*
 * reconfiguration.cpp
 *
 *  Created on: 27 nov 2015
 *      Author: andre
 */
#include <set>
#include <iostream>
#include <boost/shared_ptr.hpp>
#include <cstring>

#include "catoms2DWorld.h"
#include "scheduler.h"
#include "events.h"
#include "reconfiguration.h"
#include "reconfigurationMsg.h"
#include "catoms2DMove.h"

#define RECONFIGURATION_DEBUG
#define RECONFIGURATION_MOVES_DEBUG
#define RECONFIGURATION_MSG_DEBUG

using namespace std;
using namespace Catoms2D;

#define ROTATION_DIRECTION RelativeDirection::CW
#define OPPOSITE_ROTATION_DIRECTION RelativeDirection::getOpposite(ROTATION_DIRECTION)

#define MY_CERR cerr << "@" << catom->blockId

/***** Permieter Case State Class *****/
PerimeterCaseState::PerimeterCaseState() {
  reset(cell);
}

PerimeterCaseState::PerimeterCaseState(Coordinate c, list<reconfigurationState_t> s) {
  cell = c;
  states = s;
}

PerimeterCaseState::PerimeterCaseState(const PerimeterCaseState &pcs) {
  cell = pcs.cell;
  states = pcs.states;
}

PerimeterCaseState::~PerimeterCaseState() {}

void PerimeterCaseState::reset(Coordinate &c) {
  cell = c;
}

string PerimeterCaseState::toString() {
  string s = "(cell=" + cell.toString() + ", states=";
  list<reconfigurationState_t>::iterator it;
  for (it = states.begin(); it != states.end(); it++) {
    s += " " + Reconfiguration::toString(*it) + ",";
  }
  return s;
}

/***** Reconfiguration Class *****/

Reconfiguration::Reconfiguration(Catoms2DBlock *c, Map *m): map(m) {
  catom = c;
  state = UNKNOWN;
  started = false;
  nbWaitingAuthorization = 0;
  
  /*if (c->blockId == 1) {
    std::set<Coordinate> myset;
    std::set<Coordinate>::iterator it;
    myset.insert(Coordinate(1,0));
    myset.insert(Coordinate(1,0));
    myset.insert(Coordinate(1,0));
    std::cout << "myset contains:";
    for (it=myset.begin(); it!=myset.end(); ++it)
      std::cout << ' ' << *it;
    std::cout << '\n';
    }*/
  
}

Reconfiguration::~Reconfiguration() {}

void Reconfiguration::updatePosition() {
  Coordinate p;

  //rotationDirectionCell.reset(p);
  //antiRotationDirectionCell.reset(p);
}

std::string Reconfiguration::toString(reconfigurationState_t s) {
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

void Reconfiguration::handleStopMovingEvent() {
  // This module just stops to roll
  // Pivot was:
  P2PNetworkInterface *pivot;
  
  pivot = map->getOnBorderNeighborInterface(OPPOSITE_ROTATION_DIRECTION);

  if (pivot == NULL) {
    MY_CERR << "ERROR: ex-pivot NULL!" << endl;
    return;
  }

  // previous coordinate was:
  P2PNetworkInterface *previousPositionInt = catom->getNextInterface(ROTATION_DIRECTION, pivot, false);
  Coordinate previousPosition = map->getPosition(previousPositionInt);
  ReconfigurationStopMovingMsg *msg = new ReconfigurationStopMovingMsg(previousPosition);

#ifdef RECONFIGURATION_MSG_DEBUG
  MY_CERR << " sends STOP_MOVING to " << pivot->connectedInterface->hostBlock->blockId << endl;
#endif
  pivot->send(msg);
  
  // converge or continue to move ?
  updateState();
  if (state == WAITING) {
    queryStates();
  }
  
}

void Reconfiguration::handle(MessagePtr m) {
  ReconfigurationMsg_ptr rm = boost::static_pointer_cast<ReconfigurationMsg>(m);
  P2PNetworkInterface *recv = m->destinationInterface;
 
#ifdef RECONFIGURATION_MSG_DEBUG
  int fromId = recv->connectedInterface->hostBlock->blockId;
  MY_CERR << " " << rm->toString() << " from " << fromId << endl;
#endif

  if (!started) {
    start();
  }
  
  switch(rm->subtype) {
  case ReconfigurationMsg::STATE_QUERY: {
    ReconfigurationStateQueryMsg_ptr rsqm = boost::static_pointer_cast<ReconfigurationStateQueryMsg>(m);
    Coordinate nextCoordinate;
    P2PNetworkInterface *nextP2P;
    // Query goes through the perimeter of the concerned cell following the rotation direction
    // identify next neighbor modules on the perimeter of the concerned cell
    nextP2P = catom->getNextInterface(ROTATION_DIRECTION, recv, true);
    nextCoordinate = map->getPosition(nextP2P);
    if (nextP2P != recv &&  Map::areNeighbors(nextCoordinate,rsqm->cell)) {
      // forward to this next catom on the border of the queried cell
      ReconfigurationStateQueryMsg *msg2 = new ReconfigurationStateQueryMsg (rsqm->cell);
      nextP2P->send(msg2);
    } else {
      // send back the reply. All nodes on the reply path will include their state, 
      // and the state of the node moving around them
      list<reconfigurationState_t> states;
      if (isNeighborToMoving(rsqm->cell)) {
	  states.push_front(MOVING);
      }
      states.push_front(state);
      PerimeterCaseState pcs(rsqm->cell,states);
      forwardStateUpdate(recv,pcs);
    }
  }
    break;
  case ReconfigurationMsg::STATE_UPDATE: {
    ReconfigurationStateUpdateMsg_ptr rsum = boost::static_pointer_cast<ReconfigurationStateUpdateMsg>(m);
    P2PNetworkInterface *nextP2P = catom->getNextInterface(OPPOSITE_ROTATION_DIRECTION, recv, true);
    Coordinate nextCoordinate = map->getPosition(nextP2P);

    if (state == ASK_TO_MOVE) {
      return;
    }
    
    //getchar();
    cerr << "Next Module: " << nextP2P->connectedInterface->hostBlock->blockId << endl;
    if (nextP2P == recv || !Map::areNeighbors(nextCoordinate,rsum->states.cell)) {
      // answer is for that catom, last around the cell of interest!
      cerr << "Not forwarded!" << endl;
      // Decide whether to move or not!
      updateState();
      if (state != WAITING) {
	return;
      }
      P2PNetworkInterface *pivot = canMove(rsum->states);
      if (pivot) {
#ifdef RECONFIGURATION_MOVES_DEBUG
	MY_CERR << " can move around " << pivot->connectedInterface->hostBlock->blockId << endl; 
#endif
	if (shouldMove(pivot)) {
#ifdef RECONFIGURATION_MOVES_DEBUG
	  MY_CERR << " should move around it!" << endl; 
#endif
	  nbWaitingAuthorization = advertiseBeforeMoving();
	  state = ASK_TO_MOVE;
	} else {
	  hasConverged();
	  P2PNetworkInterface *p2p = catom->getNextInterface(ROTATION_DIRECTION, pivot, false);
	  Coordinate cell = map->getPosition(p2p);
	  list<reconfigurationState_t> states;
	  states.push_front(state);
	  PerimeterCaseState pcs(cell,states);
	  forwardStateUpdate(pivot,pcs);
	}
      }
    } else {
      // add catom's state, and states of catom moving around this one, then forward the msg
      cerr << "Forwarded!" << endl;
      PerimeterCaseState pcs = rsum->states;
      if (isNeighborToMoving(pcs.cell)) {
	pcs.states.push_front(MOVING);
	printMoving();
      }
      pcs.states.push_front(state);
      forwardStateUpdate(nextP2P,pcs);
    }
  }
    break;
  case ReconfigurationMsg::START_MOVING: {
    Coordinate c = map->getPosition(recv);
    moving.push_back(c);
    ReconfigurationStartMovingAckMsg *msg = new ReconfigurationStartMovingAckMsg();
    recv->send(msg);
  }
    break;
  case ReconfigurationMsg::START_MOVING_ACK: {
    nbWaitingAuthorization--;
    if (nbWaitingAuthorization == 0) {
      move();
    }
  }
    break;
  case ReconfigurationMsg::STOP_MOVING: {
    // This module was the pivot
    // Update state for the cell of interet for the next rolling
    // catoms on the perimeter
    ReconfigurationStopMovingMsg_ptr rsmm = boost::static_pointer_cast<ReconfigurationStopMovingMsg>(m);
    P2PNetworkInterface *oppd = NULL;
    Coordinate celloppd;
    
    // remove from moving
    Coordinate& cell = rsmm->cell;
    cerr << "Before removing" << cell << endl;
    printMoving();
    removeMoving(cell);
    cerr << "After removing" << cell << endl;
    printMoving();
    
    /*updateState();
    if (state == WAITING) {
      queryStates();
      }*/
    
    // forward stop moving to cells neigh
    oppd = catom->getNextInterface(OPPOSITE_ROTATION_DIRECTION, recv, true);

    //if (catom->blockId == 6) {
    //  cerr << "ICIIIIIIIIIIIIIIIIIII" << endl;
    //  getchar();
    //}
	
    
    celloppd = map->getPosition(oppd);
    if (Map::areNeighbors(cell,celloppd)) {
      forwardStopMoving(oppd, cell);
      cerr << "CASE 1" << endl;
      return;
    }

    // case 2 :
    //  o
    // o o o (last o was moving)
    //if (oppd == catom->getNextInterface(ROTATION_DIRECTION, recv, true)) {
    //  cerr << "CASE 2" << endl;
    //  return;
    //}
    
    // inform potentially waiting modules
    P2PNetworkInterface *cellInt = map->getInterface(cell);
    P2PNetworkInterface *potentialInterestInt =  catom->getNextInterface(OPPOSITE_ROTATION_DIRECTION, cellInt, false);
    Coordinate potentialInterest =  map->getPosition(potentialInterestInt);
    P2PNetworkInterface *potentiallyInterestedInt = catom->getNextInterface(OPPOSITE_ROTATION_DIRECTION, potentialInterestInt, false);
    Coordinate potentiallyInterested = map->getPosition(potentiallyInterestedInt);
    
    cerr << "CASSSSSEEEEEE 3" << endl;
    cerr << "Cell: " << cell << endl;
    cerr << "Interesting: " << potentialInterest << endl;
    cerr << "Interested: " << potentiallyInterested << endl;
    
    //getchar();

    if (potentialInterest.y < 0 ||
	potentiallyInterested.y < 0 ||
	!potentiallyInterestedInt->connectedInterface) {
      return;
    }
	
    // Inform (potentially) waiting modules
    list<reconfigurationState_t> states;
    
    // case module that has moved is still neighbor of the cell "cell"
    Coordinate celln = map->getPosition(recv);
    if (Map::areNeighbors(potentialInterest,celln)) {
      states.push_front(WAITING);
    }
    
    states.push_front(state);
    PerimeterCaseState pcs(potentialInterest,states);
    forwardStateUpdate(potentiallyInterestedInt,pcs);
  }
    break; 
  default:
    cerr << "unknown reconfiguration message type" << endl;
  }
}

void Reconfiguration::start() {
  if (started) { return;}
  started = true;
#ifdef RECONFIGURATION_DEBUG
  MY_CERR
    << "(" << map->getPosition().getX() << "," << map->getPosition().getY() << ")"
    << " reconfiguration starts" << endl;
#endif
  if (!hasConverged()) {
    updateState();
    if (state == WAITING) {
      queryStates();
    }
  }
#ifdef RECONFIGURATION_DEBUG
  MY_CERR << toString(state) << endl;
#endif  
}

bool Reconfiguration::hasConverged() {
  if (isInTarget()) {
#ifdef RECONFIGURATION_DEBUG
    MY_CERR << " has converged!" << endl;
#endif
    state = GOAL;
    catom->setColor(GREEN);
    return true;
  }
  return false;
}

void Reconfiguration::updateState() {
  if (state == GOAL) {
    return;
  }
  
  if (isFree() && moving.empty()) {
    state = WAITING;
    catom->setColor(RED);
  } else {
    state = BLOCKED;
    catom->setColor(GREY);
  }
}

//static Coordinate getPosition(Catoms2DBlock* c, Catoms2DMove &m) {
//  P2PNetworkInterface *p2p =  border->getInterface(ROTATION_DIRECTION);
//  Coordinate position(m.getPivot()->position[0], m.getPivot()->position[2]);
//  p2p = nextInterface(m.getPivot(),m.getDirection(),p2p);
//  return Map::getPosition(m.getPivot(),position,p2p);
//}

int Reconfiguration::advertiseBeforeMoving() {
  P2PNetworkInterface *p2p; 
  ReconfigurationStartMovingMsg *msg;
  int sent = 0;
  
  // send start to move to all neighbors
  for (int i = 0; i < 6; i++) {
    p2p = catom->getInterface((NeighborDirection::Direction)i);
    if(p2p->connectedInterface) {
      msg = new ReconfigurationStartMovingMsg();
      p2p->send(msg);
      sent++;
    }
  }
  return sent;
}

void Reconfiguration::move() {
  P2PNetworkInterface *pivot = getPivot();
  Catoms2DMove m((Catoms2DBlock*)pivot->connectedInterface->hostBlock, ROTATION_DIRECTION);

  // change map coordinate
  Coordinate p = getCellAfterRotationAround(pivot);
  map->setPosition(p);
  
  // start to move around the pivot
  catom->startMove(m);
}


void Reconfiguration::queryStates() {
  P2PNetworkInterface *pivot = getPivot();
  
  if (pivot == NULL) {
    cerr << "@" << catom->blockId << " No potential Pivot!" << endl;
    return;
  }
#ifdef RECONFIGURATION_DEBUG
  MY_CERR << " potential pivot: "
	  << pivot->connectedInterface->hostBlock->blockId
	  << endl;
#endif
  // Cell of interest:
  Coordinate cell = getCellAfterRotationAround(pivot);
  ReconfigurationStateQueryMsg *msg = new ReconfigurationStateQueryMsg (cell);
  pivot->send(msg);
}

bool Reconfiguration::shouldMove(P2PNetworkInterface *pivot) {
  Coordinate c1 = map->getPosition(); 
  Coordinate c2 = getCellAfterRotationAround(pivot);

  if (c2.y < 0) {
    return false;
  }
  if (!Map::isInTarget(c1) || (Map::isInTarget(c1) && Map::isInTarget(c2))) {
    return true;
  }
  return false;
}

P2PNetworkInterface* Reconfiguration::canMove(PerimeterCaseState &pcs) {
  P2PNetworkInterface* pivot = getPivot();
  Coordinate cell = getCellAfterRotationAround(pivot);
  list<reconfigurationState_t>::iterator it;
  int movingOrWaiting = 0;

  if (cell != pcs.cell) {
#ifdef RECONFIGURATION_DEBUG
    MY_CERR << "ERROR: cell != received states cell..." << endl;
#endif
    return NULL;
  }
   
  if (pcs.states.size() < 4) {
    // enum reconfigurationState_t {NOT_SET = -2, UNKNOWN = -1, BLOCKED, WAITING, MOVING, GOAL};
    for (it = pcs.states.begin(); it != pcs.states.end(); it++) {
      if (*it == WAITING || *it == MOVING) {
	movingOrWaiting++;
      }
    }
    if (movingOrWaiting > 0) {
      return NULL;
    } else {
      return pivot;
    }
  } else if (pcs.states.size() == 4) {
    // ask this module to move
    return NULL;
  } else {
    // Violation of algorithm assumption!
    MY_CERR << "ERROR: more than 4 cells around an empty cell!" << endl;
    return NULL;
  }
  return NULL;
}

bool Reconfiguration::isInTarget() {
  return Map::isInTarget(map->getPosition());
}

bool Reconfiguration::isOnBorder() {
  return (catom->nbNeighbors(true) <= 5);
}

bool Reconfiguration::isFree() {
  // at least 3 empty faces!
  return (catom->nbConsecutiveEmptyFaces(true) >= 3);
}

//int Reconfiguration::getFaceNum(P2PNetworkInterface *p) {
//  return (int) catom->getDirection(p);
//}

P2PNetworkInterface* Reconfiguration::getPivot() {
  return map->getOnBorderNeighborInterface(ROTATION_DIRECTION);
}

Coordinate Reconfiguration::getCellAfterRotationAround(P2PNetworkInterface *pivot) {
  P2PNetworkInterface *cellInt = catom->getNextInterface(OPPOSITE_ROTATION_DIRECTION, pivot, false);
  return map->getPosition(cellInt);
}

Catoms2DMove* Reconfiguration::nextMove() {
  if (isFree()) {
    Catoms2DBlock* pivot = map->getOnBorderNeighbor(ROTATION_DIRECTION);
    return new Catoms2DMove(pivot,ROTATION_DIRECTION);
  }
  return NULL;
}

void Reconfiguration::forwardStateUpdate(P2PNetworkInterface *p2p, PerimeterCaseState &pcs) {
  ReconfigurationStateUpdateMsg *msg = new ReconfigurationStateUpdateMsg (pcs);
  p2p->send(msg);
}

void Reconfiguration::forwardStopMoving(P2PNetworkInterface *p2p, Coordinate &c) {
  ReconfigurationStopMovingMsg *msg = new ReconfigurationStopMovingMsg (c);
  p2p->send(msg);
}

bool Reconfiguration::isDone() {
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

bool Reconfiguration::isMoving(Coordinate &c) {
  list<Coordinate>::iterator it;
  for (it = moving.begin(); it != moving.end(); it++) {
    if (*it == c) {
      return true;
    }
  }
  return false;
}

bool Reconfiguration::isNeighborToMoving(Coordinate &c) {
  list<Coordinate>::iterator it;
  for (it = moving.begin(); it != moving.end(); it++) {
    if (Map::areNeighbors(*it,c)) {
      return true;
    }
  }
  return false;
}

void Reconfiguration::removeMoving(Coordinate &c) {
  list<Coordinate>::iterator it;
  for (it = moving.begin(); it != moving.end(); it++) {
    if (*it == c) {
      moving.erase(it);
      break;
    }
  }
}


void Reconfiguration::printMoving() {
  cerr << "Moving:";
  for (list<Coordinate>::iterator it = moving.begin(); it != moving.end(); it++) {
    cerr << " " << *it;
  }
  cerr << endl;
}
