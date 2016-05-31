#ifndef CATOMS2D_DIRECTION_H
#define CATOMS2D_DIRECTION_H

#include <string>

namespace Catoms2D {

#define MAX_NB_NEIGHBORS 6
  //enum direction_t {CW = -1, CCW = 1};

class NeighborDirection {
public:
	enum Direction {Right = 0, TopRight = 1, TopLeft = 2, Left = 3, BottomLeft = 4, BottomRight = 5};
	static int getOpposite(int d);
	static std::string getString(int d);
};

class RelativeDirection {
 public:
  enum Direction {CW = -1, CCW =1};
  static inline Direction getOpposite(Direction d) { return (Direction) ((-1)*d);}
  //static string getString(direction_t d);
};

}

#endif
