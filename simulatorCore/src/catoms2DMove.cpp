#include "catoms2DMove.h"

namespace Catoms2D {

  Catoms2DMove::Catoms2DMove(Catoms2DBlock *p, RelativeDirection::Direction d) {
    pivot = p;
    direction = d;
  }

  Catoms2DMove::Catoms2DMove(const Catoms2DMove &m) {
    pivot = m.pivot;
    direction = m.direction;
  }

  Catoms2DMove::~Catoms2DMove() { }

  RelativeDirection::Direction Catoms2DMove::getDirection() {
    return direction;
  }

  Catoms2DBlock* Catoms2DMove::getPivot() {
    return pivot;
  }
}
