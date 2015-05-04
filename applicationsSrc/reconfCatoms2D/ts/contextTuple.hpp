#ifndef CONTEXT_TUPLE_H
#define CONTEXT_TUPLE_H

#include <string>
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
typedef uint64_t Time;

class ContextTuple : public Tuple {
public:
  template<typename ... Types>
  ContextTuple(Coordinate c, Types ... fs) : Tuple(c,0,fs...) {};

  template<typename ... Types>
  ContextTuple(Coordinate c, Time t,Types ... fs) : Tuple(c,t,fs...) {};

  // unable to call Tuple(t), runtime error. 
  ContextTuple(const ContextTuple& t) : Tuple() {
    fields = t.fields;
  };

  ~ContextTuple() {}

  Coordinate getLocation() {return get<Coordinate>(0);}; 
  Time getTime() {return get<Time>(1);};
};

#endif
