/*
 * catom2D1BlockCode.cpp
 *
 *  Created on: april 2015
 *      Author: andre
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
#include <float.h>

using namespace std;
using namespace Catoms2D;

//#define GEO_ROUTING_DEBUG
//#define GEO_ROUTING_TEST
//#define TEST_GEO_ROUTING_ALL_TO_ALL
//#define TEST_GEO_ROUTING_ONE_TO_ONE

//#define ANGLE_DEBUG
//#define TUPLE_DEBUG
//#define TEST_GHT

#define SEND_TARGET_TUPLES

Catoms2D1BlockCode::Catoms2D1BlockCode(Catoms2DBlock *host):Catoms2DBlockCode(host), map(host), gpsr(host,map), ctuples(gpsr,map) {
  scheduler = Catoms2D::getScheduler();
  catom2D = (Catoms2DBlock*)hostBlock;
  geoTest = false;
}

Catoms2D1BlockCode::~Catoms2D1BlockCode() {}

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

  if (!map.isConnected && (catom2D->position[2] == 0)) {
    map.connectToHost();
  }
  
  //updateBorder();
}

void Catoms2D1BlockCode::processLocalEvent(EventPtr pev) {

  stringstream info;
  switch (pev->eventType) {
  case EVENT_NI_RECEIVE: {
    MessagePtr message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
    P2PNetworkInterface * recv_interface = message->destinationInterface;
    switch(message->type) {
    case GO_MAP_MSG:
    case BACK_MAP_MSG:
      {
	bool finished = map.handleMessage(message);
	if (finished) {
	  ctuples.out(ContextTuple(string("map"), map.getPosition()));
	  if (map.connectedToHost) {
	  cout << "@" << catom2D->blockId << " is receiving the target map and disseminating it..." << endl;
	  // Link to PC host simulation:	  
#ifdef TEST_GHT
	  //out(new ContextTuple(Coordinate(6,0), string("target")));
#endif

#ifdef GEO_ROUTING_TEST
#ifdef TEST_GEO_ROUTING_ALL_TO_ALL
	  // send a packet to everybody
	  Catoms2DWorld *world = Catoms2DWorld::getWorld();
	  int *gridSize = world->getGridSize();
	  for (int iy = 0; iy < gridSize[2]; iy++) {
	    for (int ix = 0; ix < gridSize[0]; ix++) {
	      Catoms2DBlock* c = world->getGridPtr(ix,0,iy);
	      if (c != NULL) {
		Coordinate real(ix,iy);
		Coordinate t =  map.real2Virtual(real);
		cout << "@" << catom2D->blockId
		     << " (" << map.position << ")"
		     << " sends \"testGeoRouting\" tuple to @" 
		     << c->blockId << " (" <<  real << "=>" << t << ")" 
		     << endl;
		ctuples.out(ContextTuple(string("testGeoRouting"),t));
	      }
	    }
	  }
#endif
#endif

#ifdef SEND_TARGET_TUPLES
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
#endif
	  }
	}
      }
      break;
    case GPSR_PACKET: {
      MessagePtr m =  gpsr.handleGPSRPacket(message);
      if (m != NULL) {
	switch(m->type) {
	case CTUPLES_MSG: {
	    ctuples.handleCTuplesMessage(m);
	    CTuple *t = ctuples.localInp(Tuple(string("target"), map.getPosition()));
	    if (t != NULL) {
	      reconfiguration.setState(Reconfiguration::WELL_PLACED);
	      catom2D->setColor(GREEN);
	      //delete t;
	    }
	}
	    break;
	}
      }
    }
      break;
    default:
      cerr << "unknown message type" << endl;
    }
  }
    break;
  case EVENT_TRY_TO_MOVE: {
    // identify a free target position
    // 
    //in(new ContextTuple(, string("testGeoRouting")));
  }
    break;
  case  EVENT_TUPLE_QUERY_RESPONSE: {
    /*ContextTuple *tuple = (boost::static_pointer_cast<TupleQueryResponseEvent>(pev))->getTuple();
    if (tuple == NULL) {
      cout << "not found" << endl;
    } else  {
      cout << "found: " << tuple << endl;
      }*/
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

// Motion
void Catoms2D1BlockCode::startMotion(int direction, Catoms2DBlock *pivot) {
  scheduler->schedule(new MotionStartEvent(scheduler->now(),catom2D,pivot,direction));
}

Catoms2D::Catoms2DBlockCode* Catoms2D1BlockCode::buildNewBlockCode(Catoms2DBlock *host) {
  return(new Catoms2D1BlockCode(host));
}

