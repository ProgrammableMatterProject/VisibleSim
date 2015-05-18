#include "angle.h"

using namespace Catoms2D;

Angle::Angle(Catoms2D::Catoms2DBlock *host, Map &m): map(m){
  catom2D = host;
}

Angle::Angle(Angle const &a): map(a.map) {
  catom2D = a.catom2D;
}

Angle::~Angle() {}

// counterwise clock angle abc
double Angle::ccwAngle(Coordinate &a, Coordinate &b, Coordinate &c) const {
  Coordinate v1;
  Coordinate v2;
  
  v1.x = a.x - b.x;
  v1.y = a.y - b.y;
  
  v2.x = c.x - b.x;
  v2.y = c.y - b.y;

  int dot = v1.x*v2.x + v1.y*v2.y;      // dot product
  int det = v1.x*v2.y - v1.y*v2.x;      // determinant
  double angle = atan2(det, dot) * 180 / PI;

  if (det < 0) {
    return -angle;
  }
  else {
    return 360-angle;
  }
}

P2PNetworkInterface* Angle::getNextCounterClockWiseInterface(Coordinate a) {
  double angles[6] = {0,0,0,0,0,0};
  int minI = 0;
  for (int i=0; i < 6; i++) {
    P2PNetworkInterface *p2p = catom2D->getInterface((NeighborDirection::Direction)i);
    if (p2p->connectedInterface == NULL) {
      angles[i] = DBL_MAX;
      continue;
    }
    Coordinate c = map.getPosition(p2p);
    angles[i] = ccwAngle(c,map.position,a);
#ifdef DEBUG_ANGLE
    cout << c << " " << angles[i] << endl;
#endif
    if (angles[i] < angles[minI]) {
      minI = i;
    }
  }
  return catom2D->getInterface((NeighborDirection::Direction)minI);
}

P2PNetworkInterface* Angle::getNextCounterClockWiseInterface(P2PNetworkInterface *recv) {
  int d = catom2D->getDirection(recv);
  P2PNetworkInterface *next = NULL;
  do {
    d = (d+1)%6;
    next = catom2D->getInterface((NeighborDirection::Direction)d);
  } while (next->connectedInterface == NULL);
  return next;
}

P2PNetworkInterface* Angle::getNextClockWiseInterface(P2PNetworkInterface *recv) {
  int d = catom2D->getDirection(recv);
  P2PNetworkInterface *next = NULL;
  cout << "arrival interface: " << d << endl;
  do {
    if (d == 0) { 
      d=5; 
    } else { 
      d--;
    }
    next = catom2D->getInterface((NeighborDirection::Direction)d);
  } while (next->connectedInterface == NULL);
  return next;
}
