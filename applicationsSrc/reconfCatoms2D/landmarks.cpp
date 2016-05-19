#include "landmarks.h"

/**** Landmark Class ****/

Landmark::Landmark() {
  set(0,0,Coordinate(),0);
}

Landmark::Landmark(int i, int d, Coordinate p, Time t) {
  set(i, d, p, t);
}

Landmark::Landmark(const Landmark &l) {
  set(l.id, l.distance, l.position, l.time);
}

Landmark::~Landmark() {}

void Landmark::update(int i, int d, Coordinate p, Time t) {
  set(i, d, p, t);
}

void Landmark::set(int i, int d, Coordinate p, Time t) {
  id = i;
  distance = d;
  position = p;
  time = t;
}

/**** Landmarks Class ****/

Landmarks::Landmarks() {}
Landmarks::~Landmarks() {}

bool Landmarks::handleMessage(MessagePtr message) {
  P2PNetworkInterface * recv_interface = message->destinationInterface;

  switch(message->type) {
  case LANDMARK_BEACON_MSG:
    {
      // Update record

      // Broadcast message (distance+1)
      
      return true;
    }
  default:
    return false;
  }

  return false;
}

void Landmarks::handleEvent(EventPtr pev) {
  //switch (pev->eventType) {
  //}
}
