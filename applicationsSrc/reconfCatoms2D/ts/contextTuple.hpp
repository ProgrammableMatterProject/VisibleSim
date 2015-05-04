#ifndef CONTEXT_TUPLE_H
#define CONTEXT_TUPLE_H

#include <string>
#include <iostream>

class Coordinate {
private:
  int x;
  int y;
public:
  Coordinate(int x_, int y_): x(x_), y(y_) {};
  Coordinate(const Coordinate &c): x(c.x), y(c.y) {};
  inline int getX() const {return x;}
  inline int getY() const {return y;}
  ~Coordinate() {}

  bool operator ==(const Coordinate &c) const {
    return (x == c.x) && (y == c.y);
  }    
  
  friend std::ostream& operator<<(std::ostream& os, const Coordinate &c) {
    os << "(" << c.x << "," << c.y << ")";
    return os;
  }
};

class ContextTuple : public Tuple {
public:
  template<typename ... Types>
  ContextTuple(Coordinate c, Types ... fs) : Tuple(c,fs...) {};
  
  // unable to call Tuple(t), runtime error. 
  ContextTuple(const ContextTuple& t) : Tuple() { fields = t.fields; };

  ~ContextTuple() {}

   Coordinate getCoordinate() { return get<Coordinate>(0);}; 
};

#endif
