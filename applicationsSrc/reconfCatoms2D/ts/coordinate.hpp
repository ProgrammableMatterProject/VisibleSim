#ifndef COORDINATE_H_
#define COORDINATE_H_

#include <iostream>

class Coordinate {
public:
  int x;
  int y;

  Coordinate() {x=0; y=0;};
  Coordinate(int x_, int y_): x(x_), y(y_) {};
  Coordinate(const Coordinate &c): x(c.x), y(c.y) {};
  inline int getX() const {return x;}
  inline int getY() const {return y;}
  ~Coordinate() {}

  bool operator ==(const Coordinate &c) const {
    return (x == c.x) && (y == c.y);
  }    
  
  bool operator !=(const Coordinate &c) const {
    return (x != c.x) || (y != c.y);
  }
  
  friend std::ostream& operator<<(std::ostream& os, const Coordinate &c) {
    os << "(" << c.x << "," << c.y << ")";
    return os;
  }
};

#endif
