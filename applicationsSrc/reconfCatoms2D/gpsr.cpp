#include "gpsr.h"
#include "catoms2DBlock.h"
#include "segment.h"
#include "catoms2DWorld.h"

//#define GEO_ROUTING_DEBUG

using namespace Catoms2D;

GPSR::GPSR(Catoms2D::Catoms2DBlock *host, Map &m): catom2D(host), map(m), angle(catom2D, map) {}

GPSR::GPSR(GPSR const &g): catom2D(g.catom2D), map(g.map), angle(g.angle) {}

GPSR::~GPSR() {}

void GPSR::send(Coordinate s, Coordinate d, Message *msg) {
  GPSRPacket *m = new GPSRPacket(s,d,msg);
  P2PNetworkInterface *next = map.getClosestInterface(m->getDestination(), m->destinationInterface);
  if(next != NULL) {
#ifdef GEO_ROUTING_DEBUG
    cout << "Greedy forward to " << map.getPosition(next) << endl;
#endif
    send(m,next);
  } else {
    // perimeter mode
    m->setPerimeterMode(map.getPosition());
    // find interface
    next = angle.getNextConnectedCounterClockWiseInterface(m->getDestination());
    m->setFirstEdge(catom2D->getDirection(next));
    send(m,next);
#ifdef GEO_ROUTING_DEBUG
    cout << "Perimeter (new) forward to " << map.getPosition(next) << endl;
#endif
  }
}

void GPSR::send(GPSRPacket *m, P2PNetworkInterface *p2p) {
  p2p->send(m);
}

void GPSR::send(GPSRPacket_ptr m, P2PNetworkInterface *p2p) {
  GPSRPacket *msg = new GPSRPacket(m.get());
  send(msg,p2p);
}

 MessagePtr GPSR::handleGPSRPacket(MessagePtr msg) {
  GPSRPacket_ptr m = std::static_pointer_cast<GPSRPacket>(msg);
  P2PNetworkInterface *recv_interface = msg->destinationInterface;

#ifdef GEO_ROUTING_DEBUG
  cout << "Geo message: s=" << m->getSource() << " d=" << m->getDestination() << " l=" << map.getPosition(recv_interface) << " p=" <<  map.getPosition() << " (" << catom2D->blockId << ")" << endl;
#endif
  if (map.getPosition() == m->getDestination()) {
    // message well arrived
#ifdef GEO_ROUTING_DEBUG
    cout << "msg had a safe trip! " << m->getDestination() << endl;
#endif
    return m->getData();
  } else {
    // message is not arrived
    switch (m->getMode()) {
    case GPSRPacket::mode_t::GREEDY:
      {
	P2PNetworkInterface *next = map.getClosestInterface(m->getDestination(), recv_interface);
	if(next != NULL) {
#ifdef GEO_ROUTING_DEBUG
	  cout << "Greedy forward to " << map.getPosition(next) << endl;
#endif
	  send(m,next);
	} else {
	  // perimeter mode
	  m->setPerimeterMode(map.getPosition());
	  // find interface
	  next = angle.getNextConnectedCounterClockWiseInterface(m->getDestination());
	  m->setFirstEdge(catom2D->getDirection(next));
	  send(m,next);
#ifdef GEO_ROUTING_DEBUG
	  cout << "Perimeter (new) forward to " << map.getPosition(next) << endl;
#endif
	  //cout << next << " " <<  catom2D->getDirection(next) << " " << m->getFirstEdge() << endl;
	}
      }
      break;
    case GPSRPacket::mode_t::PERIMETER: {
      P2PNetworkInterface *next = angle.getNextConnectedCounterClockWiseInterface(recv_interface);
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

#ifdef GEO_ROUTING_DEBUG
	cout << "Greedy (leave perimeter) forward to " << map.getPosition(next) << endl;
#endif
	send(m,next);
      } else {
	// check if an incident edge hit/cut the segment 
	// (destination;point enter in perimeter mode)
	Segment s(m->getDestination(),m->getPerimeterStart()); 
	P2PNetworkInterface *p2p = s.getIntersectInterface(catom2D,map,NULL);
	// cout << m->getPerimeterStart() << " vs " << getPosition(p2p) << endl; 
	      
	if ((p2p != NULL) && (map.getPosition() != m->getPerimeterStart()) && (map.getPosition(p2p) != m->getPerimeterStart())) {
	  Coordinate p = map.getPosition(p2p);
	  P2PNetworkInterface *next = angle.getNextConnectedCounterClockWiseInterface(p);
	  m->setFirstEdge(catom2D->getDirection(next));
	  send(m,next);
#ifdef GEO_ROUTING_DEBUG
	  cout << "Perimeter (new face?) forward to " << map.getPosition(next) << endl;
#endif
	} else {
	  P2PNetworkInterface *next = angle.getNextConnectedCounterClockWiseInterface(recv_interface);
	  if ((m->getPerimeterStart() == map.getPosition()) && (catom2D->getDirection(next) == m->getFirstEdge())) {

#ifdef GEO_ROUTING_DEBUG
	    Catoms2DWorld *world = Catoms2DWorld::getWorld();
	    Coordinate s = map.virtual2Real(m->getSource());
	    Coordinate d = map.virtual2Real(m->getDestination());
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
#endif
	    return m->getData();
	  } else {
	    send(m,next);
#ifdef GEO_ROUTING_DEBUG
	    cout << "Perimeter (same face) forward to " << map.getPosition(next) << endl;
#endif
	  }
	}
      }
    }
    }
  }
return NULL;
}
