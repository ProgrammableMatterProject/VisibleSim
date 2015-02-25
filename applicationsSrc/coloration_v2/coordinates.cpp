#include "coordinates.h"

Coordinates::Coordinates( int arg_x, int arg_y) {
  x = arg_x;
  y = arg_y;
}

Coordinates::Coordinates( int arg_x, int arg_y, NeighborDirection arg_dir) {
  x = arg_x;
  y = arg_y;
  AdaptToBeSentBy( arg_dir);
}

void Coordinates::AdaptToBeSentBy( NeighborDirection direction) {
  switch( direction) {
    case North: {
      y--;
      break;
    }
    case East: {
      x--;
      break;
    }
    case South: {
      y++;
      break;
    }
    case West: {
      x++;
      break;
    }
    default: {
      cout << "Direction = " << direction << endl;
      assert( false);
    }
  }
}

bool Coordinates::operator==( const Coordinates other) const {
  if ( other.x != x || other.y != y) { return false; }
  return true;
}

bool Coordinates::operator!=( const Coordinates other) const {
  if ( other.x != x || other.y != y) { return true; }
  return false;
}

bool Coordinates::operator<( const Coordinates other) const {
  if ( x < other.x) { return true;  }
  if ( other.x < x) { return false; }
  if ( y < other.y) { return true;  }
  return false;
}
