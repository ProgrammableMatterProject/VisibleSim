#ifndef CATOMS2DMOVE_H
#define CATOMS2DMOVE_H

#include "catoms2DBlock.h"

namespace Catoms2D {

class Catoms2DMove {
public:
    Catoms2DBlock *pivot;
    RelativeDirection::Direction direction;

    Catoms2DMove(Catoms2DBlock *p, RelativeDirection::Direction d);
    Catoms2DMove(const Catoms2DMove &m);
    ~Catoms2DMove();

    RelativeDirection::Direction getDirection();
    Catoms2DBlock* getPivot();

};

}
#endif
