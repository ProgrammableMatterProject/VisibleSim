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

#define GEO_ROUTING_DEBUG
//#define GEO_ROUTING_TEST
//#define TEST_GEO_ROUTING_ALL_TO_ALL
//#define TEST_GEO_ROUTING_ONE_TO_ONE

//#define ANGLE_DEBUG
//#define TUPLE_DEBUG
//#define SEND_TARGET_TUPLES
//#define TEST_GHT

Catoms2D1BlockCode::Catoms2D1BlockCode(Catoms2DBlock *host):Catoms2DBlockCode(host), map(host), angle(host,map) {
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
  
  updateBorder();
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
	  localTuples.out(new ContextTuple(map.getPosition(), string("map")));
	  if (map.connectedToHost) {
	  cout << "@" << catom2D->blockId << " is receiving the target map and disseminating it..." << endl;
	  // Link to PC host simulation:
	  
#ifdef TEST_GHT
	  out(new ContextTuple(Coordinate(6,0), string("target")));
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
		Coordinate t =  real2Virtual(real,ccth);
		cout << "to @" << c->blockId << " " <<  real << " " << t << endl;
		out(new ContextTuple(t, string("testGeoRouting")));
	      }
	    }
	  }
#endif
#endif
#ifdef SEND_TARGET_TUPLES
	  /*  Catoms2DWorld *world = Catoms2DWorld::getWorld();
	      int *gridSize = world->getGridSize();
	      for (int iy = 0; iy < gridSize[2]; iy++) {
	      for (int ix = 0; ix < gridSize[0]; ix++) {
	      if (world->getTargetGrid(ix,0,iy) == fullCell ) {
		Coordinate t(ix,iy);
		t.x -= ccth.x;
		t.y -= ccth.y;
		cout << "(" << t.x << " " << t.y << ")" << endl;
		out(new ContextTuple(Coordinate(t.x,t.y), string("target")));
		//localTuples.out(Tuple(string("target"), ix, iy));
		//tuples.out(new Tuple(string("aaa"), 5, 12.5));
		//Tuple query(string("aaa"), TYPE(int), 12.5);  
		//Tuple *res = tuples.inp(query);
		}
		}
		}*/
#endif
	  }
	}
      }
      break;
    case GEO_TUPLE_MSG: {
      GeoMessage_ptr m = boost::static_pointer_cast<GeoMessage>(message); 
#ifdef GEO_ROUTING_DEBUG
      // cout << "Geo message: " << m->getSource() << " -> ... ->  " << m->getLast() << " -> ... -> " << position << " -> ... -> " <<  m->getDestination() <<  endl;
      cout << "Geo message: s=" << m->getSource() << " d=" << m->getDestination() << " l=" << map.getPosition(recv_interface) << " p=" <<  map.getPosition() << " (" << catom2D->blockId << ")" << endl;
#endif
      if (map.getPosition() == m->getDestination()) {
	// message well arrived
	cout << "msg had a safe trip! " << m->getDestination() << endl;

	handleGeoMessage(m);
#ifdef GEO_ROUTING_DEBUG
	//cout << "msg had a safe trip!" << endl;
#endif
#ifdef GEO_ROUTING_TEST
	#ifdef TEST_GEO_ROUTING_ALL_TO_ALL
	if (!geoTest && (m->getTuple().getName() == string("testGeoRouting"))) {
	  geoTest = true;
	  Catoms2DWorld *world = Catoms2DWorld::getWorld();
	  int *gridSize = world->getGridSize();
	  for (int iy = 0; iy < gridSize[2]; iy++) {
	    for (int ix = 0; ix < gridSize[0]; ix++) {
	      Catoms2DBlock* c = world->getGridPtr(ix,0,iy);
	      if (c != NULL) {
		Coordinate real(ix,iy);
		Coordinate t =  map.real2Virtual(real);
		cout << "to @" << c->blockId << " " <<  real << " " << t << endl;
		out(new ContextTuple(t, string("testGeoRouting")));
		//localTuples.out(Tuple(string("target"), ix, iy));
		//tuples.out(new Tuple(string("aaa"), 5, 12.5));
		//Tuple query(string("aaa"), TYPE(int), 12.5);  
		//Tuple *res = tuples.inp(query);
	      }
	    }
	  }
	  }
	#endif
#endif
      } else {
	// message is not arrived
	switch (m->getMode()) {
	case GeoMessage::mode_t::GREEDY:
	  {
	    P2PNetworkInterface *next = map.getClosestInterface(m->getDestination(), recv_interface);
	    if(next != NULL) {
#ifdef GEO_ROUTING_DEBUG
	      cout << "Greedy forward to " << map.getPosition(next) << endl;
#endif
	      forward(m,next);
	    } else {
	      // perimeter mode
	      m->setPerimeterMode(map.getPosition());
	      // find interface
	      next = angle.getNextCounterClockWiseInterface(m->getDestination());
	      m->setFirstEdge(catom2D->getDirection(next));
	      forward(m,next);
 #ifdef GEO_ROUTING_DEBUG
	  cout << "Perimeter (new) forward to " << map.getPosition(next) << endl;
#endif
	  //cout << next << " " <<  catom2D->getDirection(next) << " " << m->getFirstEdge() << endl;
	    }
	  }
	  break;
	case GeoMessage::mode_t::PERIMETER: {
	  /*if (m->getPerimeterStart() == position) {
	    // packet has made a complete tour
	    cout << "packet has made a complete tour (" 
		 << m->getTuple() 
		 << ")"
		 << endl;
		 } else {*/
	P2PNetworkInterface *next = angle.getNextCounterClockWiseInterface(recv_interface);
	if (next == NULL) {
	break; // next is NULL only if the catom is not connected to any other.
      }
	int d1 = map.distance(map.getPosition(next), m->getDestination());
	int d2 = map.distance(m->getPerimeterStart(),m->getDestination());
	
#ifdef GEO_ROUTING_DEBUG
	    cout << "perimeter leave?: " << d1 << "vs" << d2 << endl;
#endif
	    if ((d1 < d2) || (map.getPosition(next) == m->getDestination())) {
	      // leave PERIMETER mode
	      m->setGreedyMode();
	     
	      //if(next != NULL) {
#ifdef GEO_ROUTING_DEBUG
		cout << "Greedy (leave perimeter) forward to " << map.getPosition(next) << endl;
#endif
		forward(m,next);
	    } else {
	      // check if an incident edge hit/cut the segment 
	      // (destination;point enter in perimeter mode)
	      Segment s(m->getDestination(),m->getPerimeterStart()); 
	      P2PNetworkInterface *p2p = s.getIntersectInterface(catom2D,map,NULL);
	      // cout << m->getPerimeterStart() << " vs " << getPosition(p2p) << endl; 
	      
	      if ((p2p != NULL) && (map.getPosition() != m->getPerimeterStart()) && (map.getPosition(p2p) != m->getPerimeterStart())) {
		Coordinate p = map.getPosition(p2p);
		P2PNetworkInterface *next = angle.getNextCounterClockWiseInterface(p);
		m->setFirstEdge(catom2D->getDirection(next));
		forward(m,next);
#ifdef GEO_ROUTING_DEBUG
		cout << "Perimeter (new face?) forward to " << map.getPosition(next) << endl;
#endif
	      } else {
		P2PNetworkInterface *next = angle.getNextCounterClockWiseInterface(recv_interface);
		//	cout << next << " " <<  catom2D->getDirection(next) << " " << m->getFirstEdge() << endl;
		if ((m->getPerimeterStart() == map.getPosition()) && (catom2D->getDirection(next) == m->getFirstEdge())) {
		  Catoms2DWorld *world = Catoms2DWorld::getWorld();
		  Coordinate s = map.virtual2Real(m->getSource());
		  Coordinate d = map.virtual2Real(m->getDestination());
		  cout << "source:" << m->getSource() << " " << s << endl;
		  cout << "dest:" << m->getDestination() << " " << d << endl;
		  Catoms2DBlock *cs = world->getGridPtr(s.getX(),0,s.getY());
		  Catoms2DBlock *cd = world->getGridPtr(d.getX(),0,d.getY());
		  cs->setColor(RED);
		  int ids = cs->blockId;
		  int idd = -1;
		  
		  catom2D->setColor(BLUE);

		  if (cd != NULL) {
		    cd->setColor(GREEN);
		    idd = cd->blockId;
		  }
		  
	    // packet has made a complete tour
	    cout << "packet has made a complete tour (s="
		 << m->getSource()
		 << "=>" << s
		 << "(id=" << ids << ")"
		 << ", d="
		 << m->getDestination()
		 << "=>" << d
		 << "(id=" << idd << ")"
		 << ", p="
		 << map.getPosition()
		 << "=>" <<  map.virtual2Real(map.getPosition())
		 <<  "(id=" << catom2D->blockId << ")"
		 << endl;
	    //getchar();
	    handleGeoMessage(m);

		} else {
		  forward(m,next);
#ifdef GEO_ROUTING_DEBUG
		  cout << "Perimeter (same face) forward to " << map.getPosition(next) << endl;
#endif
		}
	      }
	    }
	}
	  break;
	default:
	  cerr << "unknown mode" << endl;
	}
	//getchar();
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
    ContextTuple *tuple = (boost::static_pointer_cast<TupleQueryResponseEvent>(pev))->getTuple();
    if (tuple == NULL) {
      cout << "not found" << endl;
    } else  {
      cout << "found: " << tuple << endl;
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
void Catoms2D1BlockCode::handleGeoMessage(GeoMessage_ptr m) {
  //{STORE = 0, QUERY, ANSWER};
  switch(m->getDataMode()) {
  case GeoMessage::STORE: {
    localOut(new ContextTuple(m->getTuple()));
  }
    break;
  case GeoMessage::QUERY:
    {
      ContextTuple q = m->getTuple();
      ContextTuple *r = localInp(&q);
      GeoMessage * msg = new GeoMessage(map.getPosition(),m->getSource(),*r,GeoMessage::ANSWER);
      send(msg);
    }
    break;
  case GeoMessage::ANSWER: {
    ContextTuple *r = new ContextTuple(m->getTuple());
    scheduler->schedule(new TupleQueryResponseEvent(scheduler->now(),catom2D,r));
  }
    break;
  }
}

void Catoms2D1BlockCode::localOut(ContextTuple *t) {
  localTuples.out(t);
}

ContextTuple* Catoms2D1BlockCode::localInp(ContextTuple *t) {
  return (ContextTuple*) localTuples.in(t);
}

void Catoms2D1BlockCode::out(ContextTuple *t) {
#ifdef TUPLE_DEBUG
  cout << "insert tuple: " << *t << endl;
#endif
 // tuple should maybe stored locally
  if (t->getLocation() == map.getPosition()) {
    localOut(t);
  } else {
    // or remotely, send the tuple
    GeoMessage * msg = new GeoMessage(map.getPosition(),t->getLocation(),*t,GeoMessage::STORE);
    send(msg);
  }
}

void Catoms2D1BlockCode::inp(ContextTuple *t) {
#ifdef TUPLE_DEBUG
  cout << "insert tuple: " << *t << endl;
#endif
  // first try locally
  ContextTuple *r  = localInp(t);
  if (r != NULL) {
    scheduler->schedule(new TupleQueryResponseEvent(scheduler->now(),catom2D,r));
  } else {
    // tuple is maybe stored locally
    if (t->getLocation() == map.getPosition()) {
      // no such tuple
      scheduler->schedule(new TupleQueryResponseEvent(scheduler->now(),catom2D,NULL));
    } else {
      // tuple is maybe be stored remotely
#ifdef TUPLE_DEBUG
      cout << "remotely" << endl;
#endif
      GeoMessage * msg = new GeoMessage(map.getPosition(),t->getLocation(),*t,GeoMessage::STORE);
      send(msg);
    }
  }
}

// Geo-routing
void Catoms2D1BlockCode::send(GeoMessage *m) {
  P2PNetworkInterface *next = map.getClosestInterface(m->getDestination(), m->destinationInterface);
  if(next != NULL) {
#ifdef GEO_ROUTING_DEBUG
    cout << "Greedy forward to " << map.getPosition(next) << endl;
#endif
    forward(m,next);
  } else {
    // perimeter mode
    m->setPerimeterMode(map.getPosition());
    // find interface
    next = angle.getNextCounterClockWiseInterface(m->getDestination());
    m->setFirstEdge(catom2D->getDirection(next));
    forward(m,next);
#ifdef GEO_ROUTING_DEBUG
    cout << "Perimeter (new) forward to " << map.getPosition(next) << endl;
#endif
  }
}

void Catoms2D1BlockCode::forward(GeoMessage *m, P2PNetworkInterface *p2p) {
  scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now(), m, p2p));
}

void Catoms2D1BlockCode::forward(GeoMessage_ptr m, P2PNetworkInterface *p2p) {
  GeoMessage * msg = new GeoMessage(m.get());
  forward(msg,p2p);
}


// Motion
void Catoms2D1BlockCode::startMotion(int direction, Catoms2DBlock *pivot) {
  scheduler->schedule(new MotionStartEvent(scheduler->now(),catom2D,pivot,direction));
}

Catoms2D::Catoms2DBlockCode* Catoms2D1BlockCode::buildNewBlockCode(Catoms2DBlock *host) {
  return(new Catoms2D1BlockCode(host));
}

