#ifndef COORDINATES_H_
#define COORDINATES_H_

#include "smartBlocksBlock.h"

using namespace SmartBlocks;

struct Coordinates {
  int x;
  int y;

  Coordinates( int, int);
  Coordinates( int, int, NeighborDirection);

  void AdaptToBeSentBy( NeighborDirection);

  bool operator==( const Coordinates) const;
  bool operator!=( const Coordinates) const;
  bool operator< ( const Coordinates) const;
};

#endif // COORDINATES_H_
