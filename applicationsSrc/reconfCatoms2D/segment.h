#ifndef SEGMENT_H_
#define SEGMENT_H_

#include "coordinate.hpp"
#include "catoms2DBlock.h"
#include "network.h"
#include "map.h"

class Segment {
 protected:
  Coordinate e1;
  Coordinate e2;

  bool onSegment(Coordinate p, Coordinate q, Coordinate r) const;
  int orientation(Coordinate p, Coordinate q, Coordinate r) const;
  
 public:

  Segment(Coordinate p1, Coordinate p2);
  Segment(const Segment &s);
  ~Segment();

  bool intersect(Segment &s) const;
  
  P2PNetworkInterface* 
    getIntersectInterface(Catoms2D::Catoms2DBlock *catom2D,
			  Map &map, 
			  P2PNetworkInterface *ignore) const;
};

#endif
