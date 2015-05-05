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

#define MAP_DEBUG
#define TUPLE_DEBUG

Coordinate Catoms2D1BlockCode::ccth;

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
    Coordinate c = Coordinate(0,0);
    setPosition(c);
    ccth.x = catom2D->position[0];
    ccth.y = catom2D->position[2];
    buildMap();
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
	Coordinate c = m->getPosition(); //getPosition(toHost, m->getLast());
	setPosition(c);
	#ifdef MAP_DEBUG
	Coordinate real;
	real.x = catom2D->position[0];
	real.y = catom2D->position[2];
	real.x -= ccth.x;
	real.y -= ccth.y;
	cout << "@" << catom2D->blockId <<  " position " << position << " vs " << real << endl;
	#endif
	waiting = 0;
	buildMap();
	if (waiting==0) {
	  mapBuilt(toHost);
	}
      } else {
	mapBuilt(recv_interface);
      }
    }
      break;
    case BACK_MAP_MSG: {
      BackMapMessage_ptr m = boost::static_pointer_cast<BackMapMessage>(message);
      waiting--;
#ifdef MAP_DEBUG
      //cout << "@" << catom2D->blockId <<  " back msg " << waiting << endl;
#endif
      if (!waiting) {
	if (!connectedToHost) {
	  mapBuilt(toHost);
	} else {
	  cout << "@" << catom2D->blockId << " is receiving the target map and disseminating it..." << endl;
	  // Link to PC host simulation:
	  Catoms2DWorld *world = Catoms2DWorld::getWorld();
	  int *gridSize = world->getGridSize();
	  for (int iy = 0; iy < gridSize[2]; iy++) {
	    for (int ix = 0; ix < gridSize[0]; ix++) {
	      if (world->getTargetGrid(ix,0,iy) == fullCell ) {
		cout << "(" << ix << " " << iy << ")" << endl;
		out(new ContextTuple(Coordinate(ix,iy), string("target")));
	    //localTuples.out(Tuple(string("target"), ix, iy));
	    //tuples.out(new Tuple(string("aaa"), 5, 12.5));
	    //Tuple query(string("aaa"), TYPE(int), 12.5);  
	    //Tuple *res = tuples.inp(query);
	      }
	    }
	  }
	}
      }
    }
      break;
    case GEO_TUPLE_MSG: {
      GeoMessage_ptr m = boost::static_pointer_cast<GeoMessage>(message); 
      cout << "geo" << endl;
      // update my position
      //Coordinate p = getPosition(recv_interface, m->getLast());
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

Coordinate Catoms2D1BlockCode::getPosition(P2PNetworkInterface *it) {
  Coordinate p = position;
  switch(catom2D->getDirection(it)) {
  case NeighborDirection::BottomLeft:
    if (p.y%2 == 0) {
      p.x--;
    }
    p.y--;
    break;
  case NeighborDirection::Left:
    p.x--;
    break;
  case NeighborDirection::TopLeft:    
    if (p.y%2 == 0) {
      p.x--;
    }
    p.y++;
    break;
  case NeighborDirection::TopRight:
    if (p.y%2 == 1) {
      p.x++;
    }
    p.y++;
    break;
  case NeighborDirection::Right:
    p.x++;
    break;
  case NeighborDirection::BottomRight: 
    if (p.y%2 == 1) {
      p.x++;
    }
    p.y--;
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
    GoMapMessage * msg = new GoMapMessage(getPosition(p2p));
    scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now(), msg, p2p));
    waiting++;
  }
}

void Catoms2D1BlockCode::mapBuilt(P2PNetworkInterface *d) {
  BackMapMessage * msg = new BackMapMessage();
  scheduler->schedule( new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now(), msg, d));
}

void Catoms2D1BlockCode::setPosition(Coordinate p) {
  position = p;
  localTuples.out(new ContextTuple(position, string("map")));
  positionKnown = true;
}

//bool legalMove(int sens, 
