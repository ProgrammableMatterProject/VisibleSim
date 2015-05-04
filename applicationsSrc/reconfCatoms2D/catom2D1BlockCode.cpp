/*
 * catom2D1BlockCode.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include <sstream>
#include "catom2D1BlockCode.h"
#include "scheduler.h"
#include "events.h"
#include "catoms2DEvents.h"
//MODIF NICO
#include <boost/shared_ptr.hpp>

#include "reconfCatoms2DMessages.h"
#include "reconfCatoms2DEvents.h"

using namespace std;
using namespace Catoms2D;

Catoms2D1BlockCode::Catoms2D1BlockCode(Catoms2DBlock *host):Catoms2DBlockCode(host) {
  scheduler = Catoms2D::getScheduler();
  catom2D = (Catoms2DBlock*)hostBlock;
  connectedToHost = false;
  waiting = 0;
  toHost = NULL;
  positionKnown = false;
}

Catoms2D1BlockCode::~Catoms2D1BlockCode() {
}

void Catoms2D1BlockCode::updateBorder() {
  for (int i = 0; i < MAX_NB_NEIGHBORS; i++) {
    if (catom2D->getInterface((NeighborDirection::Direction)i)->connectedInterface == NULL) {
      catom2D->setColor(RED);
      return;
    }
  }
  catom2D->setColor(GREEN);
}

bool Catoms2D1BlockCode::canMove() {
  int nbNeighbors = catom2D->nbNeighbors();
  int nbConsecutiveNeighbors = catom2D->nbConsecutiveNeighbors();
  return ((nbNeighbors == 1) || ((nbNeighbors == 2) && (nbConsecutiveNeighbors == 2)));
}

void Catoms2D1BlockCode::startup() {
  stringstream info;
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  int *gridSize = world->getGridSize();
  info << "Starting ";
  scheduler->trace(info.str(),hostBlock->blockId);
  
  //updateBorder();
  /*if (canMove()) {
    catom2D->setColor(RED);
    } else {
    catom2D->setColor(GREEN);
    }*/
  //catom2D->setColor(DARKGREY);
	
  /*if (catom2D->blockId == 10) {
    cout << "@" << catom2D->blockId << " " << catom2D->position << endl;
    startMotion(ROTATE_LEFT,world->getBlockById(8));
    }
	
    if (catom2D->blockId == 8) {
    cout << "@" << catom2D->blockId << " " << catom2D->position << endl;
    startMotion(ROTATE_LEFT,world->getBlockById(7));
    }*/
	
  /*if (catom2D->blockId == 4) {
    cout << "@" << catom2D->blockId << " " << catom2D->position << endl;
    startMotion(ROTATE_LEFT,world->getBlockById(3));
    }*/
		
  if (catom2D->blockId == 1) {
    connectedToHost = true;
    toHost = NULL;
  }
  
  if(connectedToHost) {
    position = Coordinate(0,0);
    positionKnown = true;
    buildMap();
    /*cout << "Catom2D 1 receiving the target map..." << endl;
    int i = 0;
    for (int iy = 0; iy < gridSize[2]; iy++) {
      for (int ix = 0; ix < gridSize[0]; ix++) {
	if (world->getTargetGrid(ix,0,iy) == fullCell ) {
	  cout << "(" << ix << " " << iy << ")" << endl;
	  if (i == 0) {
	    localTuples.out(new ContextTuple(Coordinate(ix,iy), string("map")));
	    //localTuples.out(new Tuple(string("coordinate"), ix, iy));
	  }
	  out(new ContextTuple(Coordinate(ix,iy), string("target")));
	  //localTuples.out(Tuple(string("target"), ix, iy));
	  //tuples.out(new Tuple(string("aaa"), 5, 12.5));
	  //Tuple query(string("aaa"), TYPE(int), 12.5);  
	  //Tuple *res = tuples.inp(query);
	}
      }
      }*/
  }
}

int cpt = 1;

void Catoms2D1BlockCode::processLocalEvent(EventPtr pev) {
  int i;
  MessagePtr message;
  stringstream info;
  
  switch (pev->eventType) {
  case EVENT_NI_RECEIVE: {
    message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
    P2PNetworkInterface * recv_interface = message->destinationInterface;
    switch(message->type) {
    case GO_MAP_MSG: {
      GoMapMessage_ptr m = boost::static_pointer_cast<GoMapMessage>(message);
      if (!positionKnown) {
	toHost = recv_interface;
	position  = getPosition(toHost, m->getLast());
	cout << "@" << catom2D->blockId <<  " position " << position << endl;
	positionKnown = true;
	buildMap();
	if (!waiting) {
	  mapBuilt();
	}
      }
    }
      break;
    case BACK_MAP_MSG: {
      BackMapMessage_ptr m = boost::static_pointer_cast<BackMapMessage>(message);
      waiting--;
      if (!waiting) {
	if (!connectedToHost) {
	  mapBuilt();
	} else {
	  cout << "propagate target map" << endl;
	}
      }
      }
      break;
    case GEO_TUPLE_MSG: {
      GeoMessage_ptr m = boost::static_pointer_cast<GeoMessage>(message); 
      cout << "geo" << endl;
      // update my position
      Coordinate p = getPosition(recv_interface, m->getLast());
      // check correctness of the position
    
      // 
      /*if (p != position) {
	//cout << "wrong position computation?" << endl;
	position = p;
	cout << "position " << p << endl;
	}*/

      if (m->getSource() == m->getDestination()) {
	cout << "msg had a safe trip!" << endl;
      }
    }
      break;
    default:
      cerr << "unknown message type" << endl;
    }
  }
    break;
  case EVENT_MOTION_END: {
    cout << "motion end" << endl;
    cout << "@" << catom2D->blockId << " " << catom2D->position << endl;
    catom2D->setColor(DARKGREY);
    break;
  }
  }
}

// TS
void Catoms2D1BlockCode::out(ContextTuple *t) {
  cout << "insert tuple: " << *t << endl;
}

// Geo-routing
//void Catoms2D1BlockCode::forward(Tuple *t) {
//}

void Catoms2D1BlockCode::startMotion(int direction, Catoms2DBlock *pivot) {
  scheduler->schedule(new MotionStartEvent(scheduler->now(),catom2D,pivot,direction));
}

Catoms2D::Catoms2DBlockCode* Catoms2D1BlockCode::buildNewBlockCode(Catoms2DBlock *host) {
  return(new Catoms2D1BlockCode(host));
}

Coordinate Catoms2D1BlockCode::getPosition(P2PNetworkInterface *recv_it, Coordinate c) {
  Coordinate p = c;
  
  switch(catom2D->getDirection(recv_it)) {
  case NeighborDirection::BottomLeft:
    p.y++;
    if (p.y%2 == 0) {
      p.x++;
    }
    break;
  case NeighborDirection::Left:
    p.x++;
    break;
  case NeighborDirection::TopLeft:
    p.y--;    
    if (p.y%2 == 0) {
      p.x++;
    }
    break;
  case NeighborDirection::TopRight:
    p.y--;
    if (p.y%2 == 1) {
      p.x--;
    }
    break;
  case NeighborDirection::Right:
    p.x--;
    break;
  case NeighborDirection::BottomRight:
    p.y++; 
    if (p.y%2 == 1) {
      p.x--;
    }
    break;
  }
  
  return p;
}

void Catoms2D1BlockCode::buildMap() {
  P2PNetworkInterface *p2p;
  for (int i=0; i<6; i++) {
    p2p = catom2D->getInterface((NeighborDirection::Direction)i);
    if( (p2p == toHost) || !p2p->connectedInterface) {
      continue;
    }
    GoMapMessage * msg = new GoMapMessage(position);
    scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now(), msg, p2p));
    waiting++;
  }
}

void Catoms2D1BlockCode::mapBuilt() {
  BackMapMessage * msg = new BackMapMessage();
  scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now(), msg, toHost));
}
