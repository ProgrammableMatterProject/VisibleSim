#ifndef ANGLE_H_
#define ANGLE_H_

#include <math.h>
#include "coordinate.h"
#include "network.h"
#include "map.h"
#include "catoms2DBlock.h"

#define PI 3.14159265

class Angle {
 public:
  Catoms2D::Catoms2DBlock *catom2D;
  Map &map;

  Angle(Catoms2D::Catoms2DBlock *host, Map &m);
  Angle(Angle const &a);
  ~Angle();

  // counterwise clock angle abc
  double ccwAngle(Coordinate &a, Coordinate &b, Coordinate &c) const;
  P2PNetworkInterface* getNextConnectedClockWiseInterface(P2PNetworkInterface *recv); 
  P2PNetworkInterface* getNextConnectedCounterClockWiseInterface(P2PNetworkInterface *recv);
  P2PNetworkInterface* getNextConnectedCounterClockWiseInterface(Coordinate a); 

  P2PNetworkInterface* getNextClockWiseInterface(P2PNetworkInterface *recv); 
  P2PNetworkInterface* getNextCounterClockWiseInterface(P2PNetworkInterface *recv);

};

#endif
