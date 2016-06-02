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
  string s = "(cell=" + cell.toString() + ",";
  list<reconfigurationState_t>::iterator it;
  for (it = states.begin(); it != states.end(); it++) {
    s += " " + Reconfiguration::toString(*it);
  }
  return s;
}

/***** Reconfiguration Class *****/

Reconfiguration::Reconfiguration(Catoms2DBlock *c, Map *m): map(m) {
  catom = c;
  state = UNKNOWN;
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
  P2PNetworkInterface *pivot = map->getOnBorderNeighborInterface(OPPOSITE_ROTATION_DIRECTION);                              ;
  ReconfigurationStopMovingMsg *msg = new ReconfigurationStopMovingMsg();
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
 
#ifdef RECONFIGURATION_DEBUG
  int fromId = recv->connectedInterface->hostBlock->blockId;
  MY_CERR << " " << rm->toString() << "from " << fromId << endl;
#endif
  
  switch(rm->subtype) {
  case ReconfigurationMsg::STATE_QUERY: {
    ReconfigurationStateQueryMsg_ptr rsqm = boost::static_pointer_cast<ReconfigurationStateQueryMsg>(m);
    Coordinate nextCoordinate;
    P2PNetworkInterface *nextP2P;
    // Query goes through the perimeter of the concerned cell following the rotation direction
    // identify next neighbor modules on the perimeter of the concerned cell
    nextP2P = catom->getNextInterface(ROTATION_DIRECTION, recv, true);
    nextCoordinate = map->getPosition(nextP2P);
    if (Map::areNeighbors(nextCoordinate,rsqm->cell)) {
      // forward to this next catom on the border of the queried cell
      ReconfigurationStateQueryMsg *msg2 = new ReconfigurationStateQueryMsg (rsqm->cell);
      nextP2P->send(msg2);
    } else {
      // send back the reply. All nodes on the reply path will include their state, 
      // and the state of the node moving around them
      list<reconfigurationState_t> states;
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
    
    if (!Map::areNeighbors(nextCoordinate,rsum->states.cell)) {
      // answer is for that catom, last around the cell of interest!

      // Decide whether to move or not!
      updateState();
      if (state == WAITING) {
	return;
      }
      P2PNetworkInterface *pivot = canMove(rsum->states);
      if (pivot) {
	if (shouldMove(pivot)) {
	  move(pivot);
	} else {
	  hasConverged();
	}
      }
    } else {
      // add catom's state, and states of catom moving around this one, then forward the msg
      PerimeterCaseState pcs = rsum->states;
      pcs.states.push_front(state);
      forwardStateUpdate(nextP2P,pcs);
    }
  }
    break;
  case ReconfigurationMsg::START_MOVING: {
    moving = recv;
  }
    break;
  case ReconfigurationMsg::STOP_MOVING: {
    // This module was the pivot
    // Update state for the cell of interet for the next rolling
    // catoms on the perimeter
    P2PNetworkInterface *p2p = catom->getNextInterface(OPPOSITE_ROTATION_DIRECTION, moving, true);
    Coordinate cell = map->getPosition(moving);
    list<reconfigurationState_t> states;
    if (p2p == catom->getNextInterface(ROTATION_DIRECTION, moving, true)) {
      return;
    }
    
    cell.x--;
    moving = NULL;
    
    states.push_front(state);
    PerimeterCaseState pcs(cell,states);
    forwardStateUpdate(p2p,pcs);
  }
    break; 
  default:
    cerr << "unknown reconfiguration message type" << endl;
  }
}

void Reconfiguration::start() {
#ifdef RECONFIGURATION_DEBUG
  cerr << "@" << catom->blockId
       << "(" << map->getPosition().getX() << "," << map->getPosition().getY() << ")"
       << " reconfiguration starts" << endl;
#endif
  if (!hasConverged()) {
    updateState();
    if (state == WAITING) {
      queryStates();
    }
  }
}

bool Reconfiguration::hasConverged() {
  if (isInTarget()) {
#ifdef RECONFIGURATION_DEBUG
    cerr << "@" << catom->blockId << " is in target!" << endl;
#endif
    state = GOAL;
    catom->setColor(GREEN);
    return true;
  }
  return false;
}

void Reconfiguration::updateState() {
  if (isFree()) {
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

void Reconfiguration::move(P2PNetworkInterface *pivot) {
  Catoms2DMove m((Catoms2DBlock*)pivot->connectedInterface->hostBlock, ROTATION_DIRECTION);

  // send start to move to the pivot!
  ReconfigurationStartMovingMsg *msg = new ReconfigurationStartMovingMsg();
  pivot->send(msg);
  // start to move around the pivot
  catom->startMove(m);
}

void Reconfiguration::queryStates() {
  P2PNetworkInterface *pivot = getPivot();
  if (pivot == NULL) {
    cerr << "@" << catom->blockId << " No potential Pivot!" << endl;
    return;
  }

  // Cell of interest:
  Coordinate cell = getCellAfterRotationAround(pivot);
  ReconfigurationStateQueryMsg *msg = new ReconfigurationStateQueryMsg (cell);
  pivot->send(msg);
}

bool Reconfiguration::shouldMove(P2PNetworkInterface *pivot) {
  Coordinate c1 = map->getPosition(); 
  Coordinate c2 = getCellAfterRotationAround(pivot);
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
  P2PNetworkInterface *cellInt = catom->getNextInterface(ROTATION_DIRECTION, pivot, false);
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
