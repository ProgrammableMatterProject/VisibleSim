#ifndef NEIGHBOR_H_
#define NEIGHBOR_H_

#include "comm/network.h"
#include "coordinate.h"

class Neighbor {
 public:
  P2PNetworkInterface *interface;
  Coordinate position;

  Neighbor();
  Neighbor(P2PNetworkInterface *i, Coordinate &p);
  Neighbor(const Neighbor &n);
  ~Neighbor();

  std::string toString();
};


#endif
