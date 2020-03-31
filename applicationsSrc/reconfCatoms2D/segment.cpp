#include "segment.h"

// Source:
//http://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/

using namespace std;
using namespace Catoms2D;

Segment::Segment(Coordinate p1, Coordinate p2) {
  e1 = p1;
  e2 = p2;
}

Segment::Segment(const Segment &s) {
  e1 = s.e1;
  e2 = s.e2;
}

Segment::~Segment() {}

// Given three colinear points p, q, r, the function checks if
// point q lies on line segment 'pr'
bool Segment::onSegment(Coordinate p, Coordinate q, Coordinate r) const
{
  if (q.x <= max(p.x, r.x) && q.x >= min(p.x, r.x) &&
      q.y <= max(p.y, r.y) && q.y >= min(p.y, r.y))
    return true;
    
  return false;
}
 
// To find orientation of ordered triplet (p, q, r).
// The function returns following values
// 0 --> p, q and r are colinear
// 1 --> Clockwise
// 2 --> Counterclockwise
int Segment::orientation(Coordinate p, Coordinate q, Coordinate r) const
{
  // See 10th slides from following link for derivation of the formula
  // http://www.dcs.gla.ac.uk/~pat/52233/slides/Geometry1x1.pdf
  int val = (q.y - p.y) * (r.x - q.x) -
    (q.x - p.x) * (r.y - q.y);
 
  if (val == 0) return 0;  // colinear
 
  return (val > 0)? 1: 2; // clock or counterclock wise
}
 
// The main function that returns true if line segment 'p1q1'
// and 'p2q2' intersect.
bool Segment::intersect(Segment &s) const
{
    
  Coordinate p1 = this->e1;
  Coordinate q1 = this->e2;
  Coordinate p2 = s.e1;
  Coordinate q2 = s.e2;

  // Find the four orientations needed for general and
  // special cases
  int o1 = orientation(p1, q1, p2);
  int o2 = orientation(p1, q1, q2);
  int o3 = orientation(p2, q2, p1);
  int o4 = orientation(p2, q2, q1);
 
  // General case
  if (o1 != o2 && o3 != o4)
    return true;
 
  // Special Cases
  // p1, q1 and p2 are colinear and p2 lies on segment p1q1
  if (o1 == 0 && onSegment(p1, p2, q1)) return true;
 
  // p1, q1 and p2 are colinear and q2 lies on segment p1q1
  if (o2 == 0 && onSegment(p1, q2, q1)) return true;
 
  // p2, q2 and p1 are colinear and p1 lies on segment p2q2
  if (o3 == 0 && onSegment(p2, p1, q2)) return true;
 
  // p2, q2 and q1 are colinear and q1 lies on segment p2q2
  if (o4 == 0 && onSegment(p2, q1, q2)) return true;
 
  return false; // Doesn't fall in any of the above cases
}

P2PNetworkInterface* Segment::getIntersectInterface(Catoms2D::Catoms2DBlock *catom2D, Map &map, P2PNetworkInterface *ignore) const {
  for (int i = 0; i < 6; i++) {
    P2PNetworkInterface *it = catom2D->getInterface(i);
    if ((ignore == it) || (it->connectedInterface == NULL)) {
      continue;
    }
    Coordinate pdir = map.getPosition(it);
    Segment seg = Segment(map.getPosition(),pdir);
    if(intersect(seg)) {
      return it;
    }
  }
  return NULL;
}
