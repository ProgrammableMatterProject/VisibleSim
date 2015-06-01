#ifndef CATOMS2DMOVE_H
#define CATOMS2DMOVE_H

namespace Catoms2D {

class Catoms2DBlock;

class Catoms2DMove {
 public:
  enum direction_t {ROTATE_CCW = -1, ROTATE_CW = 1};
  Catoms2DBlock *pivot;
  direction_t direction;

  Catoms2DMove(Catoms2DBlock *p, direction_t d);
  Catoms2DMove(const Catoms2DMove &m);
  ~Catoms2DMove();

  direction_t getDirection();
  Catoms2DBlock* getPivot();
};

}
#endif
